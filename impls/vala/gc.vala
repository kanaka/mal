abstract class GC.Object : GLib.Object {
    public GC.Object? next;
    public unowned GC.Object? prev;
    public bool visited;

    public delegate void VisitorFunc(GC.Object? obj);

    construct {
        next = null;
        prev = null;
        GC.Core.register_object(this);
    }
    public abstract void gc_traverse(VisitorFunc visitor);
}

class GC.Root : GLib.Object {
    public weak GC.Root? next;
    public weak GC.Root? prev;

    public GC.Object? obj;

    construct { GC.Core.register_root(this); }
    ~Root() { GC.Core.unregister_root(this); }

    public Root.empty() { obj = null; }
    public Root(GC.Object? obj_) { obj = obj_; }
}

class GC.Core : GLib.Object {
    private struct ObjectQueue {
        GC.Object? head;
        GC.Object? tail;

        public void unlink(GC.Object obj_) {
            GC.Object obj = obj_;

            if (obj.prev == null) {
                assert(obj == head);
                head = obj.next;
            }
            else
                obj.prev.next = obj.next;

            if (obj.next == null)
                tail = obj.prev;
            else
                obj.next.prev = obj.prev;
        }

        public void link(GC.Object obj) {
            if (tail != null) {
                tail.next = obj;
                obj.prev = tail;
            } else {
                head = obj;
                obj.prev = null;
            }

            tail = obj;
            obj.next = null;
        }
    }

    private static ObjectQueue objects;
    private static weak GC.Root? roots_head;
    private static uint until_next_collection;

    static construct {
        objects.head = objects.tail = null;
        roots_head = null;
    }

    public static void register_object(GC.Object obj) {
#if GC_DEBUG
        stderr.printf("GC: registered %p [%s]\n",
                      obj, Type.from_instance(obj).name());
#endif
        objects.link(obj);
        if (until_next_collection > 0)
            until_next_collection--;
    }
    public static void register_root(GC.Root root) {
#if GC_DEBUG
        stderr.printf("GC: registered root %p\n", root);
#endif
        root.next = roots_head;
        root.prev = null;
        if (roots_head != null)
            roots_head.prev = root;
        roots_head = root;
    }
    public static void unregister_root(GC.Root root) {
#if GC_DEBUG
        stderr.printf("GC: unregistered root %p\n", root);
#endif
        if (root.prev == null)
            roots_head = root.next;
        else
            root.prev.next = root.next;
        if (root.next != null)
            root.next.prev = root.prev;
    }

    private static void statistics(uint before, uint after, uint roots) {
#if GC_STATS
        stderr.printf("GC: %u roots, %u -> %u objects\n",
                      roots, before, after);
#endif
    }

    public static void collect() {
        uint orig = 0;
        uint roots = 0;

#if GC_DEBUG
        stderr.printf("GC: started\n");
#endif
        for (unowned GC.Object obj = objects.head; obj != null; obj = obj.next)
        {
            obj.visited = false;
#if GC_DEBUG
            stderr.printf("GC: considering %p [%s]\n",
                          obj, Type.from_instance(obj).name());
#endif
            orig++;
        }

        ObjectQueue after = { null, null };
        until_next_collection = 0;

        for (unowned GC.Root root = roots_head; root != null; root = root.next)
        {
            roots++;
            if (root.obj != null && !root.obj.visited) {
                GC.Object obj = root.obj;
#if GC_DEBUG
                stderr.printf("GC: root %p -> %p [%s]\n",
                              root, obj, Type.from_instance(obj).name());
#endif
                objects.unlink(obj);
                after.link(obj);
                obj.visited = true;
                until_next_collection++;
            }
        }

        for (GC.Object? obj = after.head; obj != null; obj = obj.next) {
#if GC_DEBUG
            stderr.printf("GC: traversing %p [%s]\n",
                          obj, Type.from_instance(obj).name());
#endif
            obj.gc_traverse((obj2_) => {
                GC.Object obj2 = obj2_;
                if (obj2 == null)
                    return;
                if (!obj2.visited) {
#if GC_DEBUG
                    stderr.printf("GC: %p -> %p [%s]\n",
                                  obj, obj2, Type.from_instance(obj2).name());
#endif
                    objects.unlink(obj2);
                    after.link(obj2);
                    obj2.visited = true;
                    until_next_collection++;
                }
            });
        }

        // Manually free everything, to avoid stack overflow while
        // recursing down the list unreffing them all
        objects.tail = null;
        while (objects.head != null) {
#if GC_DEBUG
            stderr.printf("GC: collecting %p [%s]\n", objects.head,
                          Type.from_instance(objects.head).name());
#endif
            objects.head = objects.head.next;
        }

        objects = after;

#if GC_DEBUG
        stderr.printf("GC: finished\n");
#endif

        statistics(orig, until_next_collection, roots);
    }

    public static void maybe_collect() {
#if !GC_ALWAYS
        if (until_next_collection > 0)
            return;
#endif
        collect();
    }
}
