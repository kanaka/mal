include tests/common.mk
include types.mk

# treat an expression as a statement
do = $(eval __tmp := $(1))


$(info Testing foreach as a let form)

$(call assert_eq,XbX,$(foreach local_var,b,X$(local_var)X),\
    Using foreach as 'let' failed)


$(info Testing type function)
$(call assert_eq,make,$(call _obj_type,xyz),\
    (type xyz) is not 'make')
$(call assert_eq,nil,$(call _obj_type,$(__nil)),\
    (type $$(__nil)) is not 'nil')
$(call assert_eq,true,$(call _obj_type,$(__true)),\
    (type $$(__true)) is not 'true')
$(call assert_eq,false,$(call _obj_type,$(__false)),\
    (type $$(__false)) is not 'false')


$(info Testing number? function)

$(call assert_eq,number,$(call _obj_type,$(call number,1)))
$(call assert_eq,number,$(call _obj_type,$(call number,10)))
$(call assert_eq,number,$(call _obj_type,$(call number,12345)))


$(info Testing symbols)

$(call assert_eq,symbol,$(call _obj_type,$(call symbol,abc)),\
    (type (symbol abc)) is not 'symbol')
SYM1 := $(call symbol,a sym value)
$(call assert_eq,a sym value,$($(SYM1)_value))
$(call assert_eq,$(__true),$(call symbol?,$(SYM1)))


$(info Testing strings)

$(call assert_eq,string,$(call _obj_type,$(call string,abc)),\
    (type (string abc)) is not string)

STR1 := $(call string,a string value)
$(call assert_eq,a string value,$(call str_decode,$($(STR1)_value)))
$(call assert_eq,$(__true),$(call string?,$(STR1)))
$(call assert_eq,14,$(call _count,$(STR1)))

STR2 := $(call string,a string (with parens))
$(call assert_eq,a string (with parens),$(call str_decode,$($(STR2)_value)))
$(call assert_eq,$(__true),$(call string?,$(STR2)))
$(call assert_eq,22,$(call _count,$(STR2)))

$(info Testing strings (subs))
$(call assert_eq,a string (with parens),$(call str_decode,$($(call subs,$(STR2),$(call number,2))_value)))
$(call assert_eq,a string,$(call str_decode,$($(call subs,$(STR2),$(call number,0),$(call number,8))_value)))

$(info Testing strings (str))
$(call assert_eq,a string value - a string (with parens),$(call str_decode,$($(call str,$(STR1) $(call string, - ) $(STR2))_value)))


$(info Testing function objects)

$(call assert_eq,function,$(call _obj_type,$(call _function,abc)),\
    (type (function abc)) is not 'function')
FN1 := $(call _function,arg1:'$$(word 1,$$(1))' arg2:'$$(word 2,$$(1))')
$(call assert_eq,$(__true),$(call function?,$(FN1)))
$(call assert_eq,arg1:'A' arg2:'B',$(call apply,$(FN1),$(call list,A B)))


$(info Testing lists)

$(call assert_eq,list,$(call _obj_type,$(call list)),\
    (type (list)) is not 'list')

$(info Testing lists (cons))
L1 := $(call cons,P $(call list))
L2 := $(call cons,Q $(L1))
$(call assert_eq,$(__true),$(call list?,$(L1)))
$(call assert_eq,$(__true),$(call list?,$(L2)))
$(call assert_eq,P,$(call sfirst,$(L1)))
$(call assert_eq,2,$(call _count,$(L2)))
$(call assert_eq,Q,$(call sfirst,$(L2)))
$(call assert_eq,P,$(call _nth,$(L2),1))
$(call assert_eq,$(__true),$(call equal?,$(L1) $(call srest,$(L2))))

$(info Testing lists (concat))
L1_2 := $(call concat,$(L1) $(L2))
$(call assert_eq,3,$(call _count,$(L1_2)))
$(call assert_eq,P,$(call sfirst,$(L1_2)))
$(call assert_eq,Q,$(call _nth,$(L1_2),1))
$(call assert_eq,P,$(call _nth,$(L1_2),2))
$(call assert_eq,$(__true),$(call equal?,$(L2) $(call srest,$(L1_2))))

$(info Testing lists (conj))
L3 := $(call _conj!,$(call list),A B)
L4 := $(call _conj!,$(call list),X $(L3))
$(call assert_eq,$(__true),$(call list?,$(L3)),\
    (list? $$(L3)))
$(call assert_eq,$(__true),$(call list?,$(L4)),\
    (list? $$(L3)))
$(call assert_eq,A,$(call sfirst,$(L3)),\
    (sfirst $$(L3)) is not 'A')
$(call assert_eq,X,$(call sfirst,$(L4)),\
    (sfirst $$(L4)) is not 'X')
$(call assert_eq,$(__true),$(call list?,$(call _nth,$(L4),1)),\
    (_nth $$(L4),1) is not a list)
$(call assert_eq,A,$(call sfirst,$(call _nth,$(L4),1)),\
    (first (_nth $$(L4),1)) is not 'A')


$(info Testing hash_maps)

X := $(call _hash_map)
$(call assert_eq,$(__true),$(call hash_map?,$(X)),\
    (hash_map? $$(X)))
$(call assert_eq,$(__false),$(call vector?,$(X)),\
    (vector? $$(X)))

mykey := $(call _string,a)
$(call assert_not,$(call _get,$(X),a),\
    (get $$(X),a))
$(call assert_eq,$(__false),$(call contains?,$(X),$(mykey)),\
    (contains? $$(X),a))
$(call do,$(call _assoc!,$(X),a,value of X a))
$(call assert_eq,value of X a,$(call _get,$(X),a),\
    (get $$(X),a) is not 'value of Xa')
$(call assert_eq,$(__true),$(call contains?,$(X) $(mykey)),\
    (contains? $$(X),a))

Y := $(call _hash_map)
$(call assert_eq,0,$(call _count,$(Y)),\
    (_count $$(Y)))
$(call do,$(call _assoc!,$(Y),a,value of Y a))
$(call assert_eq,1,$(call _count,$(Y)),\
    (_count $$(Y)))
$(call do,$(call _assoc!,$(Y),b,value of Y b))
$(call assert_eq,2,$(call _count,$(Y)),\
    (_count $$(Y)))
$(call assert_eq,value of Y a,$(call _get,$(Y),a),\
    (get $$(Y),a) is not 'value of Y a')
$(call assert_eq,value of Y b,$(call _get,$(Y),b),\
    (get $$(Y),b) is not 'value of Y b')
$(call assert_eq,value of Y a value of Y b,$(call raw_flat,$(Y),b),\
    (raw_flat $(Y)) is not 'value of Y a value of Y b')

$(call do,$(call _assoc!,$(X),b,$(Y)))
$(call assert_eq,2,$(call _count,$(Y),a),\
    (_count $$(Y)) should still be 2)

$(call assert_eq,$(__true),$(call hash_map?,$(call _get,$(X),b)),\
    (hash_map? (get $$(X),b)))

$(call assert_eq,$(call _get,$(call _get,$(X),b),a),value of Y a,\
    (get (get $(X),b),a) is not 'value of Y a')
$(call assert_eq,$(call _get,$(call _get,$(X),b),b),value of Y b,\
    (get (get $(X),b),b) is not 'value of Y b')

$(call do,$(call _dissoc!,$(Y),a))
$(call assert_eq,1,$(call _count,$(Y)),\
    (_count $$(Y)) should now be 1)
$(call assert_not,$(call _get,$(Y),a),\
    (get $$(Y),a))
$(call do,$(call _dissoc!,$(Y),b))
$(call assert_eq,0,$(call _count,$(Y)),\
    (_count $$(Y)) should now be 0)
$(call assert_not,$(call _get,$(Y),b),\
    (get $$(Y),b))


$(info Testing vectors)

V1 := $(call _conj!,$(call vector),first.vector.value second.vector.value third.vector.value)
$(call assert_eq,$(__true),$(call vector?,$(V1)),\
    (vector? $$(V1)))
$(call assert_eq,first.vector.value,$(call _nth,$(V1),0))
$(call assert_eq,second.vector.value,$(call _nth,$(V1),1))
$(call assert_eq,third.vector.value,$(call _nth,$(V1),2))
$(call assert_eq,third.vector.value,$(call slast,$(V1)))
$(call assert_eq,3,$(call _count,$(V1)))

V2 := $(call _conj!,$(call vector),A B C)
$(call assert_eq,3,$(call _count,$(V2)),\
    (_count $$(V2)) is not 3)
$(call assert_eq,A B C,$($(V2)_value))
$(call assert_eq,A,$(call sfirst,$(V2)),\
    (first $$(V2)) is not 'A')
$(call assert_eq,$(__true),$(call list?,$(call srest,$(V2))),\
    (rest $$(V2)) is not a vector)
$(call assert_eq,B C,$($(call srest,$(V2))_value))
$(call assert_eq,B,$(call sfirst,$(call srest,$(V2))),\
    (first (rest $$(V2))) is not 'B')
$(call assert_eq,C,$(call sfirst,$(call srest,$(call srest,$(V2)))),\
    (first (rest (rest $$(V2)))) is not 'C')
$(call assert_eq,C,$(call _nth,$(V2),2),\
    (_nth $$(V2),2) is not 'C')

V2_1 := $(call _conj!,$(V2),$(V1))
$(call assert_eq,4,$(call _count,$(V2_1)),\
    (_count $$(V2_1)) is not 4)
$(call assert_eq,C,$(call _nth,$(V2_1),2),\
    (_nth $$(V2_1),2) is no longer 'C')
$(call assert_eq,$(__true),$(call vector?,$(call _nth,$(V2_1),3)),\
    (_nth $$(V2_1),3) is not a vector)
$(call assert_eq,second.vector.value,$(call _nth,$(call _nth,$(V2_1),3),1),\
    (_nth (_nth $$(V2_1),3),1) is not 'second.vector.value')

$(info Testing vectors (rest))

V3 := $(call srest,$(V2_1))
$(call assert_eq,3,$(call _count,$(V3)),\
    (_count $$(V3)) is not 3)
$(call assert_eq,B,$(call sfirst,$(V3)),\
    (first $$(V3)) is not 'B')
$(call assert_eq,$(__true),$(call vector?,$(call _nth,$(V3),2)),\
    (_nth $$(V3),2) is not a vector)
$(call assert_eq,second.vector.value,$(call _nth,$(call _nth,$(V3),2),1),\
    (_nth (_nth $$(V3),2),1) is not 'second.vector.value')

$(info Testing vectors (contains?))

$(call assert_eq,$(__true),$(call _contains?,$(V2_1),0),\
    (contains? $$(V2_1),0))
$(call assert_eq,,$(call _contains?,$(V2_1),7),\
    (contains? $$(V2_1),7))


$(info Testing _apply function)

label_args = $(word 1,$(1))$(word 2,$(1))$(word 3,$(1))$(word 4,$(1))
$(call assert_eq,,$(call _apply,label_args,$(call list)))
$(call assert_eq,A,$(call _apply,label_args,$(call list,A)))
$(call assert_eq,AB,$(call _apply,label_args,$(call list,A B)))
$(call assert_eq,ABCD,$(call _apply,label_args,$(call list,A B C D)))


$(info Testing smap function)

L5 := $(call _conj!,$(call list),$(call number,1) $(call number,2) $(call number,3))
inc = $(call number_plus,$(call number,1) $(1))
$(call assert_eq,(2 3 4),$(call _pr_str,$(call _smap,inc,$(L5))))
inc_func := $(call _function,$$(call number_plus,$$(call number,1) $$(1)))
$(call assert_eq,(2 3 4),$(call _pr_str,$(call smap,$(inc_func) $(L5))))


$(info Testing equal? function)
$(call assert_eq,$(__true),$(call equal?,2 2))
$(call assert_eq,$(__false),$(call equal?,2 3))
$(call assert_eq,$(__false),$(call equal?,2 3))
$(call assert_eq,$(__true),$(call equal?,abc abc))
$(call assert_eq,$(__false),$(call equal?,abc abz))
$(call assert_eq,$(__false),$(call equal?,zbc abc))
$(call assert_eq,$(__true),$(call equal?,$(call string,abc) $(call string,abc)))
$(call assert_eq,$(__false),$(call equal?,$(call string,abc) $(call string,abz)))
$(call assert_eq,$(__false),$(call equal?,$(call string,zbc) $(call string,abc)))
$(call assert_eq,$(__true),$(call equal?,$(call symbol,abc) $(call symbol,abc)))
$(call assert_eq,$(__false),$(call equal?,$(call symbol,abc) $(call symbol,abz)))
$(call assert_eq,$(__false),$(call equal?,$(call symbol,zbc) $(call symbol,abc)))
L6 := $(call _conj!,$(call list),1 2 3)
L7 := $(call _conj!,$(call list),1 2 3)
L8 := $(call _conj!,$(call list),1 2 Z)
L9 := $(call _conj!,$(call list),Z 2 3)
L10 := $(call _conj!,$(call list),1 2)
$(call assert_eq,$(__true),$(call equal?,$(L6) $(L7)))
$(call assert_eq,$(__false),$(call equal?,$(L6) $(L8)))
$(call assert_eq,$(__false),$(call equal?,$(L6) $(L9)))
$(call assert_eq,$(__false),$(call equal?,$(L6) $(L10)))
$(call assert_eq,$(__false),$(call equal?,$(L10) $(L6)))


$(info Testing empty? function)
$(call assert_eq,$(__true),$(call empty?,$(call list)))
$(call assert_eq,$(__false),$(call empty?,$(call list,1)))


$(info Testing ENV (1 level))
env1 := $(call ENV)
$(call assert_eq,,$(call        ENV_GET,$(env1),a))
$(call assert_eq,$(env1),$(call ENV_SET,$(env1),a,val_a))
$(call assert_eq,$(env1),$(call ENV_SET,$(env1),b,val_b))
$(call assert_eq,$(env1),$(call ENV_SET,$(env1),=,val_eq))
$(call assert_eq,val_a,$(call   ENV_GET,$(env1),a))
$(call assert_eq,val_b,$(call   ENV_GET,$(env1),b))
$(call assert_eq,val_eq,$(call  ENV_GET,$(env1),=))
$(call assert_eq,hash_map,$(call _obj_type,$(call ENV_FIND,$(env1),a)))
$(call assert_eq,val_a,$(call   _get,$(call ENV_FIND,$(env1),a),a))

$(info Testing ENV (2 levels))
env2 := $(call ENV,$(env1))
$(call assert_eq,$(env2),$(call ENV_SET,$(env2),b,val_b2))
$(call assert_eq,$(env2),$(call ENV_SET,$(env2),c,val_c))
$(call assert_eq,$(env1),$(call ENV_FIND,$(env2),a))
$(call assert_eq,$(env2),$(call ENV_FIND,$(env2),b))
$(call assert_eq,$(env2),$(call ENV_FIND,$(env2),c))
$(call assert_eq,val_a,$(call   ENV_GET,$(env2),a))
$(call assert_eq,val_b2,$(call  ENV_GET,$(env2),b))
$(call assert_eq,val_c,$(call   ENV_GET,$(env2),c))


.PHONY: all
all:
	@echo "All tests completed"
