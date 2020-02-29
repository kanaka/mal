import time
import os
import readline
import sys
import xml.etree.ElementTree as ET
from threading import Thread
from threading import Lock
from collections import deque
from multiprocessing import Process, Queue

fname = sys.argv[1]
args = sys.argv[2:]
tree = ET.Element('mal')
ET.SubElement(tree, 'stdin')

if len(args) > 0:
    args0 = args[0]
    ET.SubElement(tree, 'argv')
    for a in tree.iter('mal'):
        for a in a.iter('argv'):
            for arg in args[1:]:
                ET.SubElement(a, 'arg').text = arg

tree = ET.ElementTree(tree)
stdout = sys.stdout

try:
    readline.read_history_file('.xslt_mal_history')
except:
    pass

finished = False
sem = Lock()
init_t = time.time() * 1000
process_pool_count = 1 if len(args) > 0 else 4 # we don't need a process pool when running a single file
process_pool = None 

class SaxonProcess:
    def __init__(self, pid, xsl):
        self.pid = pid
        self.command = f'saxon -xsl:{xsl} -s:xslt_input.xml process_id={pid} > xslt_output-{pid}.xml 2> xsl_error-{pid}.xml'
        self.process = None
        self.interpreter = Thread(target=self.interpret).start()
        self.reboot()
    
    def reboot(self):
        if self.process is not None and self.process.is_alive():
            self.process.kill()
        self.queue = Queue(1)
        self.process = Process(target=self.boot)
        self.process.start()

    def interpret(self):
        while not finished:
            try:
                with open(f'xsl_error-{self.pid}.xml', 'r') as f:
                    xtree = ET.fromstring(f.read().strip('\x00'))
                    # stdout.write(xtree.attrib['kind'])
                    req = xtree
                    if req is not None:
                        if req.attrib['kind'] == 'readline':
                            stdout.write(req.attrib['value'])
                            x = input()
                            with open('xsl_input-string', 'w') as fx:
                                fx.write(x)
                            # stdout.write(' = ' + x)
                        elif req.attrib['kind'] == 'display':
                            x = req.attrib['value']
                            # stdout.write(' = ' + x)
                            stdout.write(x + '\n')
                        elif req.attrib['kind'] == 'time':
                            x = time.time() * 1000 - init_t
                            # stdout.write(' = ' + str(int(x)))
                            with open('xsl_input-string', 'w') as fx:
                                fx.write(str(int(x)))
                        # stdout.write('\n')
                        elif req.attrib['kind'] == 'xpath-eval':
                            xpath = req.attrib['value']
                            with open('xsl-eval.xslt', 'w') as f:
                                f.write(f'<?xml version="1.0" encoding="UTF-8"?><xsl:stylesheet version="3.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:fn="http://www.w3.org/2005/02/xpath-functions"  xmlns:xs="http://www.w3.org/2001/XMLSchema"  xmlns:map="http://www.w3.org/2005/xpath-functions/map" xmlns:env="ENV" xmlns:core="CORE" exclude-result-prefixes="env core xs xsl map fn"><xsl:output omit-xml-declaration="yes"/><xsl:template match="/"><xsl:sequence select="{xpath}" /></xsl:template></xsl:stylesheet>')
                            with open('xsl-null.xml', 'w') as f:
                                f.write(req.attrib['context'])

                            if os.system(f'saxon -xsl:xsl-eval.xslt -s:xsl-null.xml > xsl-eval_output.xml'):
                                x = ''
                            else:
                                with open('xsl-eval_output.xml', 'r') as f:
                                    x = f.read()
                            with open('xsl_input-string', 'w') as fx:
                                fx.write(x)
                        # stdout.write('\n')
            except Exception as e:
                # if str(e) != 'no element found: line 1, column 0':
                #     f.seek(0)
                #     print(e, list(x for x in f.read()))
                continue
            with open(f'xsl_error-{self.pid}.xml', 'w') as f:
                f.write('')
    
    def boot(self):
        self.queue.put(os.system(self.command))
    
    def exit(self):
        self.process.kill()

    def execute(self, tree, on_complete):
        tree.write(f'process-input-{self.pid}.xml')
        with open(f'process-lock-{self.pid}', 'w') as f:
            f.write('go')
        self.process.join()
        os.system(f"mv xsl_error-{self.pid}.xml xsl_error.xml")
        os.system(f"mv xslt_output-{self.pid}.xml xslt_output.xml")
        return on_complete(self.queue.get())
        

class SaxonProcessPool:
    def __init__(self, procs: iter):
        self.pool = deque(procs)
    
    def acquire(self):
        if len(self.pool) == 0:
            raise Exception("Process Pool empty, magic?")
        proc = self.pool.pop()
        return proc
    
    def release(self, proc):
        proc.reboot()
        self.pool.appendleft(proc)
    
    def exit(self):
        for proc in self.pool:
            proc.exit()

def setup_request_file(step):
    global process_pool
    os.system('rm -rf xsl_input-string')
    os.system('mkfifo xsl_input-string')
    for i in range(process_pool_count):
        os.system(f'rm -rf process-lock-{i}')
        os.system(f'mkfifo process-lock-{i}')
    
    process_pool = SaxonProcessPool(SaxonProcess(pid=i, xsl=step) for i in range(process_pool_count))
    

setup_request_file(fname)

def transform(do_print=True):
    global tree
    def on_completion(exit_code):
        if exit_code:
            with open('xsl_error.xml', 'r') as f:
                lines = f.readlines()
                if len(lines):
                    print('Error:', [x for x in lines if x.strip() != ''][-1], end='')
            return False
        else:
            try:
                with open('xsl_error.xml', 'r') as f:
                    print(f.read(), end='')
            except:
                pass

            return True

    proc = process_pool.acquire()
    ok = proc.execute(tree, on_completion)
    process_pool.release(proc)

    if not ok:
        return

    tree = ET.parse('xslt_output.xml')
    if do_print:
        stdout = ''
        for a in tree.iter('mal'):
            for a in a.iter('stdout'):
                stdout = a
        print(stdout.text)
        stdout.clear()
        del stdout


if len(args) > 0:
    for a in tree.iter('mal'):
        for a in tree.iter('stdin'):
            a.text = f'(load-file "{args0}")'
    transform(do_print=False)
else:
    if fname == 'stepA_mal.xslt':
        # prepare state
        for a in tree.iter('mal'):
                for a in a.iter('stdin'):
                    a.text = '(println (str "Mal [" *host-language* "]"))'
        transform(do_print=False)

    # repl loop
    while True:
        try:
            x = input('user> ')
        except EOFError:
            break
        except KeyboardInterrupt:
            break

        for a in tree.iter('mal'):
            for a in a.iter('stdin'):
                a.text = x

        transform()

    readline.write_history_file('.xslt_mal_history')

finished = True
process_pool.exit()