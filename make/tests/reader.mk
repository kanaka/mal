INTERACTIVE = no

include tests/common.mk
include reader.mk

_tonum = $(call int_decode,$($(1)_value))

$(info Testing READ_STR of numbers)
$(call assert_eq,2,$(call _tonum,$(call READ_STR,2)))
$(call assert_eq,12345,$(call _tonum,$(call READ_STR,12345)))
$(call assert_eq,12345,$(call _tonum,$(call READ_STR,12345 "abc")))
$(call assert_eq,12345,$(call _tonum,$(call READ_STR,12345"abc")))
$(call assert_eq,12345,$(call _tonum,$(call READ_STR,12345(1))))

$(info Testing READ_STR of symbols)
$(call assert_eq,abc,$($(call READ_STR,abc)_value))
$(call assert_eq,abc,$($(call READ_STR,abc )_value))
$(call assert_eq,abc,$($(call READ_STR,abc"a str")_value))
$(call assert_eq,abc,$($(call READ_STR,abc(2 3))_value))

$(info Testing READ_STR of strings)
$(call assert_eq,a string,$(call str_decode,$($(call READ_STR,"a string")_value)))
$(call assert_eq,a string (with parens),$(call str_decode,$($(call READ_STR,"a string (with parens)")_value)))
$(call assert_eq,a string,$(call str_decode,$($(call READ_STR,"a string"())_value)))
$(call assert_eq,a string,$(call str_decode,$($(call READ_STR,"a string"123)_value)))
$(call assert_eq,a string,$(call str_decode,$($(call READ_STR,"a string"abc)_value)))
$(call assert_eq,,$(call str_decode,$($(call READ_STR,"")_value)))
$(call assert_eq,abc ,$(call str_decode,$($(call READ_STR,"abc ")_value)))
$(call assert_eq, abc,$(call str_decode,$($(call READ_STR," abc")_value)))
$(call assert_eq,$$abc,$(call str_decode,$($(call READ_STR,"$$abc")_value)))
$(call assert_eq,abc$$(),$(call str_decode,$($(call READ_STR,"abc$$()")_value)))
$(call assert_eq,"xyz",$(call str_decode,$($(call READ_STR,"\"xyz\"")_value)))

$(info Testing READ_STR of lists)
$(call assert_eq,2,$(call _count,$(call READ_STR,(2 3))))
$(call assert_eq,2,$(call _tonum,$(call sfirst,$(call READ_STR,(2 3)))))
$(call assert_eq,3,$(call _tonum,$(call sfirst,$(call srest,$(call READ_STR,(2 3))))))
L := $(strip $(call READ_STR,(+ 1 2 "str1" "string (with parens) and 'single quotes'")))
$(call assert_eq,5,$(call _count,$(L)))
$(call assert_eq,str1,$(call str_decode,$($(call _nth,$(L),3)_value)))
$(call assert_eq,string (with parens) and 'single quotes',$(call str_decode,$($(call _nth,$(L),4)_value)))

$(info Testing READ_STR of vectors)
$(call assert_eq,2,$(call _count,$(call READ_STR,[2 3])))
$(call assert_eq,2,$(call _tonum,$(call sfirst,$(call READ_STR,[2 3]))))
$(call assert_eq,3,$(call _tonum,$(call sfirst,$(call srest,$(call READ_STR,[2 3])))))
L := $(strip $(call READ_STR,[+ 1 2 "str1" "string (with parens) and 'single quotes'"]))
$(call assert_eq,5,$(call _count,$(L)))
$(call assert_eq,str1,$(call str_decode,$($(call _nth,$(L),3)_value)))
$(call assert_eq,string (with parens) and 'single quotes',$(call str_decode,$($(call _nth,$(L),4)_value)))

$(info Testing READ_STR of quote/quasiquote)
$(call assert_eq,quote,$($(call _nth,$(call READ_STR,'1),0)_value)) #'
$(call assert_eq,1,$(call _tonum,$(call _nth,$(call READ_STR,'1),1))) #'
$(call assert_eq,quote,$($(call _nth,$(call READ_STR,'(1 2 3)),0)_value)) #'
$(call assert_eq,3,$(call _tonum,$(call _nth,$(call _nth,$(call READ_STR,'(1 2 3)),1),2))) #'

$(call assert_eq,quasiquote,$($(call _nth,$(call READ_STR,`1),0)_value))
$(call assert_eq,1,$(call _tonum,$(call _nth,$(call READ_STR,`1),1)))
$(call assert_eq,quasiquote,$($(call _nth,$(call READ_STR,`(1 2 3)),0)_value))
$(call assert_eq,3,$(call _tonum,$(call _nth,$(call _nth,$(call READ_STR,`(1 2 3)),1),2)))

$(call assert_eq,unquote,$($(call _nth,$(call READ_STR,~1),0)_value))
$(call assert_eq,1,$(call _tonum,$(call _nth,$(call READ_STR,~1),1)))
$(call assert_eq,unquote,$($(call _nth,$(call READ_STR,~(1 2 3)),0)_value))
$(call assert_eq,3,$(call _tonum,$(call _nth,$(call _nth,$(call READ_STR,~(1 2 3)),1),2)))

$(call assert_eq,splice-unquote,$($(call _nth,$(call READ_STR,~@1),0)_value))
$(call assert_eq,1,$(call _tonum,$(call _nth,$(call READ_STR,~@1),1)))
$(call assert_eq,splice-unquote,$($(call _nth,$(call READ_STR,~@(1 2 3)),0)_value))
$(call assert_eq,3,$(call _tonum,$(call _nth,$(call _nth,$(call READ_STR,~@(1 2 3)),1),2)))


.PHONY: all
all:
	@echo "All tests completed"
