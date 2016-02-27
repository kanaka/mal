#
# mal (Make a Lisp) number types
#

ifndef __mal_numbers_included
__mal_numbers_included := true

_TOP_DIR := $(dir $(lastword $(MAKEFILE_LIST)))
include $(_TOP_DIR)util.mk

LIST20_X := x x x x x x x x x x x x x x x x x x x x
LIST100_X := $(foreach x,$(LIST20_X),X X X X X)
LIST100_0 := $(foreach x,$(LIST20_X),0 0 0 0 0)
LIST100_9 := $(foreach x,$(LIST20_X),9 9 9 9 9)

###
### general numeric utility functions
###

int_encode = $(strip $(call _reverse,\
               $(eval __temp := $(1))\
               $(foreach a,- 0 1 2 3 4 5 6 7 8 9,\
                 $(eval __temp := $$(subst $$a,$$a$$(SPACE),$(__temp))))$(__temp)))

int_decode = $(strip $(call _join,$(call _reverse,$(1))))

# trim extaneous zero digits off the end (front of number)
_trim_zeros = $(if $(call _EQ,0,$(strip $(1))),0,$(if $(call _EQ,0,$(word 1,$(1))),$(call _trim_zeros,$(wordlist 2,$(words $(1)),$(1))),$(1)))
trim_zeros = $(strip \
               $(if $(call _EQ,0,$(strip $(filter-out -,$(1)))),\
                 $(filter-out -,$(1)),\
                 $(call _reverse,$(call _trim_zeros,$(call _reverse,$(filter-out -,$(1))))))\
               $(if $(filter -,$(1)), -,))

# drop the last element of a list of words/digits
drop_last = $(call _reverse,$(wordlist 2,$(words $(1)),$(call _reverse,$(1))))

### utility function tests

#$(info $(filter-out 1,$(filter 1%,1 132 456)))
#$(info (int_encode 13): [$(call int_encode,13)])
#$(info (int_encode 156463): [$(call int_encode,156463)])
#$(info (int_encode -156463): [$(call int_encode,-156463)])
#$(info (int_decode (int_encode 156463)): [$(call int_decode,$(call int_encode,156463))])

#$(info trim_zeros(0 0 0): [$(call trim_zeros,0 0 0)])
#$(info trim_zeros(0 0 0 -): [$(call trim_zeros,0 0 0 -)])


### 
### comparisons
###

# compare two digits and return 'true' if digit 1 is less than or
# equal to digit 2
_lte_digit = $(strip \
              $(if $(call _EQ,$(1),$(2)),\
                true,\
                $(if $(call _EQ,0,$(1)),\
                  true,\
                  $(if $(wordlist $(1),$(2),$(LIST20_X)),\
                    true,\
                    ))))

# compare two lists of digits (MSB->LSB) of equal length and return
# 'true' if number 1 is less than number 2
_lte_digits = $(strip \
                $(if $(strip $(1)),\
                  $(if $(call _EQ,$(word 1,$(1)),$(word 1,$(2))),\
                    $(call _lte_digits,$(wordlist 2,$(words $(1)),$(1)),$(wordlist 2,$(words $(2)),$(2))),\
                    $(if $(call _lte_digit,$(word 1,$(1)),$(word 1,$(2))),true,)),\
                  true))

### lte/less than or equal to

_int_lte_encoded = $(strip \
                     $(foreach len1,$(words $(1)),$(foreach len2,$(words $(2)),\
                     $(if $(call _EQ,$(len1),$(len2)),\
                       $(call _lte_digits,$(call _reverse,$(1)),$(call _reverse,$(2))),\
                       $(if $(wordlist $(len1),$(len2),$(LIST100_X)),\
                         true,\
                         )))))

int_lte_encoded = $(strip \
                    $(if $(filter -,$(1)),\
                      $(if $(filter -,$(2)),\
                        $(call _int_lte_encoded,$(filter-out -,$(2)),$(filter-out -,$(1))),\
                        true),\
                      $(if $(filter -,$(2)),\
                        ,\
                        $(call _int_lte_encoded,$(1),$(2)))))

int_lte = $(call int_lte_encoded,$(call int_encode,$(1)),$(call int_encode,$(2)))

### lt/less than

int_lt_encoded = $(strip \
                   $(if $(call _EQ,$(strip $(1)),$(strip $(2))),\
                     ,\
                     $(call int_lte_encoded,$(1),$(2))))

int_lt = $(call int_lt_encoded,$(call int_encode,$(1)),$(call int_encode,$(2)))

### gte/greater than or equal to

int_gte_encoded = $(strip \
                $(if $(call _EQ,$(strip $(1)),$(strip $(2))),\
                  true,\
                  $(if $(call int_lte_encoded,$(1),$(2)),,true)))

int_gte = $(call int_gte_encoded,$(call int_encode,$(1)),$(call int_encode,$(2)))

### gt/greater than

int_gt_encoded = $(strip \
               $(if $(call _EQ,$(strip $(1)),$(strip $(2))),\
                 ,\
                 $(call int_gte_encoded,$(1),$(2))))

int_gt = $(call int_gt_encoded,$(call int_encode,$(1)),$(call int_encode,$(2)))

#$(info _lte_digit,7,8: [$(call _lte_digit,7,8)])
#$(info _lte_digit,8,8: [$(call _lte_digit,8,8)])
#$(info _lte_digit,2,1: [$(call _lte_digit,2,1)])
#$(info _lte_digit,0,0: [$(call _lte_digit,0,0)])
#$(info _lte_digit,0,1: [$(call _lte_digit,0,1)])
#$(info _lte_digit,1,0: [$(call _lte_digit,1,0)])

#$(info _lte_digits,1 2 3,1 2 4: [$(call _lte_digits,1 2 3,1 2 4)])
#$(info _lte_digits,1 2 4,1 2 4: [$(call _lte_digits,1 2 4,1 2 4)])
#$(info _lte_digits,1 2 5,1 2 4: [$(call _lte_digits,1 2 5,1 2 4)])
#$(info _lte_digits,4 1,9 0: [$(call _lte_digits,4 1,9 0)])

# The main comparison operator (others are built on this)
#$(info int_lte_encoded,1,1: [$(call int_lte_encoded,1,1)])
#$(info int_lte_encoded,1,2: [$(call int_lte_encoded,1,2)])
#$(info int_lte_encoded,2,1: [$(call int_lte_encoded,2,1)])
#$(info int_lte_encoded,0,3: [$(call int_lte_encoded,0,3)])
#$(info int_lte_encoded,3,0: [$(call int_lte_encoded,3,0)])
#$(info int_lte_encoded,1 4,0 9: [$(call int_lte_encoded,1 4,0 9)])
#$(info int_lte_encoded,4 3 2 1,4 3 2 1: [$(call int_lte_encoded,4 3 2 1,4 3 2 1)])
#$(info int_lte_encoded,5 3 2 1,4 3 2 1: [$(call int_lte_encoded,5 3 2 1,4 3 2 1)])
#$(info int_lte_encoded,4 3 2 1,5 3 2 1: [$(call int_lte_encoded,4 3 2 1,5 3 2 1)])
# negative numbers
#$(info int_lte_encoded,7 -,7: [$(call int_lte_encoded,7 -,7)])
#$(info int_lte_encoded,7,7 -: [$(call int_lte_encoded,7,7 -)])
#$(info int_lte_encoded,7 -,7 -: [$(call int_lte_encoded,7 -,7 -)])
#$(info int_lte_encoded,1 7 -,0 7: [$(call int_lte_encoded,1 7 -,0 7)])
#$(info int_lte_encoded,1 7,0 7 -: [$(call int_lte_encoded,1 7,0 7 -)])
#$(info int_lte_encoded,1 7 -,0 7 -: [$(call int_lte_encoded,1 7 -,0 7 -)])
#$(info int_lte_encoded,4 3 2 1 -,4 3 2 1: [$(call int_lte_encoded,4 3 2 1 -,4 3 2 1)])
#$(info int_lte_encoded,4 3 2 1,4 3 2 1 -: [$(call int_lte_encoded,4 3 2 1,4 3 2 1 -)])
#$(info int_lte_encoded,4 3 2 1 -,4 3 2 1 -: [$(call int_lte_encoded,4 3 2 1 -,4 3 2 1 -)])

#$(info int_lte,1,1: [$(call int_lte,1,1)])
#$(info int_lte,1,2: [$(call int_lte,1,2)])
#$(info int_lte,2,1: [$(call int_lte,2,1)])
#$(info int_lte,0,3: [$(call int_lte,0,3)])
#$(info int_lte,3,0: [$(call int_lte,3,0)])
#$(info int_lte,1234,1234: [$(call int_lte,1234,1234)])
#$(info int_lte,1235,1234: [$(call int_lte,1235,1234)])
#$(info int_lte,1234,1235: [$(call int_lte,1234,1235)])
#$(info int_lte,-1234,1235: [$(call int_lte,-1234,1235)])
#$(info int_lte,1234,-1235: [$(call int_lte,1234,-1235)])
#$(info int_lte,-1234,-1235: [$(call int_lte,-1234,-1235)])

#$(info int_lt,1,1: [$(call int_lt,1,1)])
#$(info int_lt,1,2: [$(call int_lt,1,2)])
#$(info int_lt,2,1: [$(call int_lt,2,1)])
#$(info int_lt,0,3: [$(call int_lt,0,3)])
#$(info int_lt,3,0: [$(call int_lt,3,0)])
#$(info int_lt,1234,1234: [$(call int_lt,1234,1234)])
#$(info int_lt,1235,1234: [$(call int_lt,1235,1234)])
#$(info int_lt,1234,1235: [$(call int_lt,1234,1235)])
#
#$(info int_gte,1,1: [$(call int_gte,1,1)])
#$(info int_gte,1,2: [$(call int_gte,1,2)])
#$(info int_gte,2,1: [$(call int_gte,2,1)])
#$(info int_gte,0,3: [$(call int_gte,0,3)])
#$(info int_gte,3,0: [$(call int_gte,3,0)])
#$(info int_gte,1234,1234: [$(call int_gte,1234,1234)])
#$(info int_gte,1235,1234: [$(call int_gte,1235,1234)])
#$(info int_gte,1234,1235: [$(call int_gte,1234,1235)])
#
#$(info int_gt,1,1: [$(call int_gt,1,1)])
#$(info int_gt,1,2: [$(call int_gt,1,2)])
#$(info int_gt,2,1: [$(call int_gt,2,1)])
#$(info int_gt,0,3: [$(call int_gt,0,3)])
#$(info int_gt,3,0: [$(call int_gt,3,0)])
#$(info int_gt,1234,1234: [$(call int_gt,1234,1234)])
#$(info int_gt,1235,1234: [$(call int_gt,1235,1234)])
#$(info int_gt,1234,1235: [$(call int_gt,1234,1235)])
#$(info int_gt,-1234,1235: [$(call int_gt,-1234,1235)])
#$(info int_gt,-1234,-1235: [$(call int_gt,-1234,-1235)])


###
### addition
###


# add_digits_with_carry
_add_digit = $(words $(if $(strip $(1)),$(wordlist 1,$(1),$(LIST20_X)),) \
                     $(if $(strip $(2)),$(wordlist 1,$(2),$(LIST20_X)),))

# add one to a single digit
_inc_digit = $(words $(wordlist 1,$(if $(1),$(1),0),$(LIST20_X)) x)

# add two encoded numbers digit by digit without resolving carries
# (each digit will be larger than 9 if there is a carry value)
_add = $(if $(1)$(2),$(call _add_digit,$(word 1,$(1)),$(word 1,$(2))) $(call _add,$(wordlist 2,$(words $(1)),$(1)),$(wordlist 2,$(words $(2)),$(2))),)

# take the result of _add and resolve the carry values digit by digit
_resolve_carries = $(strip \
             $(if $(1),\
               $(foreach num,$(word 1,$(1)),\
                 $(if $(filter-out 1,$(filter 1%,$(num))),\
                   $(call _resolve_carries,$(call _inc_digit,$(word 2,$(1))) $(wordlist 3,$(words $(1)),$(1)),$(2) $(patsubst 1%,%,$(num))),\
                   $(call _resolve_carries,$(wordlist 2,$(words $(1)),$(1)),$(2) $(num)))),\
               $(2)))

_negate = $(strip \
            $(if $(call _EQ,0,$(strip $(1))),\
              0,\
              $(if $(filter -,$(1)),$(filter-out -,$(1)),$(1) -)))

# add two encoded numbers, returns encoded number
_int_add_encoded = $(call _resolve_carries,$(call _add,$(1),$(2)))

int_add_encoded = $(strip \
                    $(if $(filter -,$(1)),\
                      $(if $(filter -,$(2)),\
                        $(call _negate,$(call _int_add_encoded,$(filter-out -,$(1)),$(filter-out -,$(2)))),\
                        $(call int_sub_encoded,$(2),$(filter-out -,$(1)))),\
                      $(if $(filter -,$(2)),\
                        $(call int_sub_encoded,$(1),$(filter-out -,$(2))),\
                        $(call _int_add_encoded,$(1),$(2)))))

# add two unencoded numbers, returns unencoded number
int_add = $(call int_decode,$(call int_add_encoded,$(call int_encode,$(1)),$(call int_encode,$(2))))

### addition tests

#$(info _add_digit(7,6,1): [$(call _add_digit,7,6,1)])
#$(info _add_digit(7,6,0): [$(call _add_digit,7,6,0)])
#$(info _add_digit(7,6,0): [$(call _add_digit,7,6,0)])
#$(info _carries(12 14 15): [$(call _carries,12 14 15)])
#$(info _inc_digit(0): $(call _inc_digit,0))
#$(info _inc_digit(1): $(call _inc_digit,1))
#$(info _inc_digit(9): $(call _inc_digit,9))
#$(info _inc_digit(18): $(call _inc_digit,18))
#$(info int_add_encoded(0,0): [$(call int_add_encoded,0,0)])

#$(info int_add(1,2): [$(call int_add,1,2)])
#$(info int_add(9,9): [$(call int_add,9,9)])
#$(info int_add(0,9): [$(call int_add,0,9)])
#$(info int_add(9,0): [$(call int_add,9,0)])
#$(info int_add(0,0): [$(call int_add,0,0)])
#$(info int_add(123,456): [$(call int_add,123,456)])
#$(info int_add(678,789): [$(call int_add,678,789)])
#$(info int_add(1,12): [$(call int_add,1,12)])
#$(info int_add(123,5): [$(call int_add,123,5)])
#$(info int_add(123456,9): [$(call int_add,123456,9)])
#$(info int_add(999999991,9): [$(call int_add,999999991,9)])
# negative numbers
#$(info int_add(-2,2): [$(call int_add,-2,2)])
#$(info int_add(-1,2): [$(call int_add,-1,2)])
#$(info int_add(1,-2): [$(call int_add,1,-2)])
#$(info int_add(-1,-2): [$(call int_add,-1,-2)])

###
### subtraction
###

_get_zeros = $(if $(call _EQ,0,$(word 1,$(1))),$(call _get_zeros,$(wordlist 2,$(words $(1)),$(1)),$(2) 0),$(2))

# return a 9's complement of a single digit
_complement9 = $(strip \
                 $(if $(call _EQ,0,$(1)),9,\
                 $(if $(call _EQ,1,$(1)),8,\
                 $(if $(call _EQ,2,$(1)),7,\
                 $(if $(call _EQ,3,$(1)),6,\
                 $(if $(call _EQ,4,$(1)),5,\
                 $(if $(call _EQ,5,$(1)),4,\
                 $(if $(call _EQ,6,$(1)),3,\
                 $(if $(call _EQ,7,$(1)),2,\
                 $(if $(call _EQ,8,$(1)),1,\
                 $(if $(call _EQ,9,$(1)),0)))))))))))

# return a 10's complement of a single digit
_complement10 = $(call _inc_digit,$(call _complement9,$(1)))

# 
_complement_rest = $(if $(strip $(1)),\
                     $(strip \
                       $(call _complement10,$(word 1,$(1))) \
                       $(foreach digit,$(wordlist 2,$(words $(1)),$(1)),\
                         $(call _complement9,$(digit)))),)

# return the complement of a number
_complement = $(strip $(call _get_zeros,$(1)) \
                      $(call _complement_rest,$(wordlist $(call _inc_digit,$(words $(call _get_zeros,$(1)))),$(words $(1)),$(1))))

# subtracted encoded number 2 from encoded number 1 and return and
# encoded number result. both numbers must be positive but may have
# a negative result
__int_sub_encoded = $(strip \
                  $(call trim_zeros,\
                    $(call drop_last,\
                      $(call int_add_encoded,\
                        $(1),\
                        $(wordlist 1,$(words $(1)),$(call _complement,$(2)) $(LIST100_9))))))

_int_sub_encoded = $(strip \
                     $(if $(call _EQ,0,$(strip $(2))),\
                       $(1),\
                       $(if $(call _int_lte_encoded,$(2),$(1)),\
                         $(call __int_sub_encoded,$(1),$(2)),\
                         $(call _negate,$(call __int_sub_encoded,$(2),$(1))))))

int_sub_encoded = $(strip \
                    $(if $(filter -,$(1)),\
                      $(if $(filter -,$(2)),\
                        $(call _int_sub_encoded,$(filter-out -,$(2)),$(filter-out -,$(1))),\
                        $(call _negate,$(call _int_add_encoded,$(filter-out -,$(1)),$(2)))),\
                      $(if $(filter -,$(2)),\
                        $(call _int_add_encoded,$(1),$(filter-out -,$(2))),\
                        $(call _int_sub_encoded,$(1),$(2)))))

# subtract unencoded number 2 from unencoded number 1 and return
# unencoded result
int_sub = $(call int_decode,$(call int_sub_encoded,$(call int_encode,$(1)),$(call int_encode,$(2))))

### subtraction tests

#$(info _get_zeros(5 7): [$(call _get_zeros,5 7)])
#$(info _get_zeros(0 0 0 2): [$(call _get_zeros,0 0 0 2)])
#$(info _get_zeros(0 0 0 2 5): [$(call _get_zeros,0 0 0 2 5)])

#$(info _complement(0): [$(call _complement,0)])
#$(info _complement(1): [$(call _complement,1)])
#$(info _complement(9): [$(call _complement,9)])
#$(info _complement(5 7): [$(call _complement,5 7)])
#$(info _complement(0 0 0 2): [$(call _complement,0 0 0 2)])
#$(info _complement(0 0 0 5 4 3 2 1): [$(call _complement,0 0 0 5 4 3 2 1)])

#$(info int_sub_encoded(0 0 1, 3 1): [$(call int_sub_encoded,0 0 1,3 1)])
#$(info int_sub_encoded(2, 2): [$(call int_sub_encoded,2,2)])

#$(info int_sub(2,1): [$(call int_sub,2,1)])
#$(info int_sub(2,0): [$(call int_sub,2,0)])
#$(info int_sub(2,2): [$(call int_sub,2,2)])
#$(info int_sub(100,13): [$(call int_sub,100,13)])
#$(info int_sub(100,99): [$(call int_sub,100,99)])
#$(info int_sub(91,19): [$(call int_sub,91,19)])
# negative numbers
#$(info int_sub(1,2): [$(call int_sub,1,2)])
#$(info int_sub(-1,2): [$(call int_sub,-1,2)])
#$(info int_sub(1,-2): [$(call int_sub,1,-2)])
#$(info int_sub(-1,-2): [$(call int_sub,-1,-2)])
#$(info int_sub(-2,-1): [$(call int_sub,-2,-1)])
#$(info int_sub(19,91): [$(call int_sub,19,91)])
#$(info int_sub(91,-19): [$(call int_sub,91,-19)])
#$(info int_sub(-91,19): [$(call int_sub,-91,19)])
#$(info int_sub(-91,-19): [$(call int_sub,-91,-19)])


###
### multiplication
###

# multiply two digits
#_mult_digit = $(words $(foreach x,$(1),$(2)))
_mult_digit = $(strip \
                $(words $(foreach x,$(wordlist 1,$(1),$(LIST20_X)),\
                        $(wordlist 1,$(2),$(LIST20_X)))))

# multipy every digit of number 1 with number 2
# params: digits, digit, indent_zeros, results
_mult_row = $(if $(strip $(1)),$(call _mult_row,$(wordlist 2,$(words $(1)),$(1)),$(2),$(3)0,$(4) $(call _mult_digit,$(word 1,$(1)),$(2))$(3)),$(4))

# multiply every digit of number 2 with every digit of number 1 adding
# correct zero padding to the end of each result
# params: digits, digits, indent_zeros, results
_mult_each = $(if $(strip $(2)),$(call _mult_each,$(1),$(wordlist 2,$(words $(2)),$(2)),$(3)0,$(4) $(call _mult_row,$(1),$(word 1,$(2)),$(3))),$(4))

# add up a bunch of unencoded numbers. Basically reduce into the first number
_add_many = $(if $(word 2,$(1)),$(call _add_many,$(call int_add,$(word 1,$(1)),$(word 2,$(1))) $(wordlist  3,$(words $(1)),$(1))),$(1))

# multiply two encoded numbers, returns encoded number
_int_mult_encoded = $(call trim_zeros,$(call int_encode,$(call _add_many,$(call _mult_each,$(1),$(2)))))

int_mult_encoded = $(strip \
             $(if $(filter -,$(1)),\
               $(if $(filter -,$(2)),\
                 $(call _int_mult_encoded,$(filter-out -,$(1)),$(filter-out -,$(2))),\
                 $(call _negate,$(call _int_mult_encoded,$(filter-out -,$(1)),$(2)))),\
               $(if $(filter -,$(2)),\
                 $(call _negate,$(call _int_mult_encoded,$(1),$(filter-out -,$(2)))),\
                 $(call _int_mult_encoded,$(1),$(2)))))

# multiply two unencoded numbers, returns unencoded number
int_mult = $(call int_decode,$(call int_mult_encoded,$(call int_encode,$(1)),$(call int_encode,$(2))))

#$(info _mult_digit(8,6): [$(call _mult_digit,8,6)])
#$(info _mult_digit(7,6): [$(call _mult_digit,7,6)])
#$(info _mult_row(8,6): [$(call _mult_row,8,6)])
#$(info _mult_row(8 7,6): [$(call _mult_row,8 7,6)])
#$(info _mult_row(8 7 3,6): [$(call _mult_row,8 7 3,6)])
#$(info _mult_each(8 7 6, 4 3 2): [$(call _mult_each,8 7 6,4 3 2)])
#$(info _add_many(123 234 345 456): [$(call _add_many,123 234 345 456)])

#$(info int_mult_encoded(8 7 3,6): [$(call int_mult_encoded,8 7 3,6)])
#$(info int_mult_encoded(8 7 3,0): [$(call int_mult_encoded,8 7 3,0)])

#$(info int_mult(378,6): [$(call int_mult,378,6)])
#$(info int_mult(678,234): [$(call int_mult,678,234)])
#$(info int_mult(1,23456): [$(call int_mult,1,23456)])
#$(info int_mult(0,23456): [$(call int_mult,0,23456)])
#$(info int_mult(0,0): [$(call int_mult,0,0)])
# negative numbers
#$(info int_mult(-378,6): [$(call int_mult,-378,6)])
#$(info int_mult(678,-234): [$(call int_mult,678,-234)])
#$(info int_mult(-1,-23456): [$(call int_mult,-1,-23456)])
#$(info int_mult(0,-23456): [$(call int_mult,0,-23456)])

###
### division
###

# return list of zeros needed to pad number 2 to the same length as number 1
_zero_pad = $(strip $(wordlist 1,$(call int_sub,$(words $(1)),$(words $(2))),$(LIST100_0)))

# num1, num2, zero pad, result_accumulator
# algorithm:
# - B = pad with zeros to make same digit length as A
# - loop
#    - if (B <= A)
#        - A = subtract B from A
#        - C = C + 10^(B pad.length)
#    - else
#        - if B.length < origin B.length: break
#        - chop least significant digit of B
_div = $(strip \
         $(if $(call int_lte_encoded,$(3) $(2),$(1)),\
           $(call _div,$(call int_sub_encoded,$(1),$(3) $(2)),$(2),$(3),$(call int_add_encoded,$(4),$(3) 1)),\
           $(if $(3),\
             $(call _div,$(1),$(2),$(wordlist 2,$(words $(3)),$(3)),$(4)),\
             $(4))))

# divide two encoded numbers, returns encoded number
_int_div_encoded = $(strip \
                $(if $(call _EQ,0,$(1)),\
                  0,\
                  $(if $(call _EQ,$(1),$(2)),\
                    1,\
                    $(if $(call int_gt_encoded,$(2),$(1)),\
                      0,\
                      $(call _div,$(1),$(2),$(call _zero_pad,$(1),$(2)),0)))))

int_div_encoded = $(strip \
             $(if $(filter -,$(1)),\
               $(if $(filter -,$(2)),\
                 $(call _int_div_encoded,$(filter-out -,$(1)),$(filter-out -,$(2))),\
                 $(call _negate,$(call _int_div_encoded,$(filter-out -,$(1)),$(2)))),\
               $(if $(filter -,$(2)),\
                 $(call _negate,$(call _int_div_encoded,$(1),$(filter-out -,$(2)))),\
                 $(call _int_div_encoded,$(1),$(2)))))

# divide two unencoded numbers, returns unencoded number
int_div = $(call int_decode,$(call int_div_encoded,$(call int_encode,$(1)),$(call int_encode,$(2))))

### division tests

#$(info _zero_pad(1 2 3 4,1 3): [$(call _zero_pad,1 2 3 4,1 3)])
#$(info _zero_pad(1 2,1 3): [$(call _zero_pad,1 2,1 3)])
#$(info _zero_pad(2,1 3): [$(call _zero_pad,1 2,1 3)])
#
#$(info int_div_encoded(2,1): [$(call int_div_encoded,2,1)])
#$(info int_div_encoded(3,1): [$(call int_div_encoded,3,1)])
#$(info int_div_encoded(3,2): [$(call int_div_encoded,3,2)])
#$(info int_div_encoded(0,7): [$(call int_div_encoded,0,7)])
#$(info int_div_encoded(0 3,0 2): [$(call int_div_encoded,0 3,0 2)])
#$(info int_div_encoded(0 3,5): [$(call int_div_encoded,0 3,5)])
#
#$(info int_div(5,1): [$(call int_div,5,1)])
#$(info int_div(5,2): [$(call int_div,5,2)])
#$(info int_div(123,7): [$(call int_div,123,7)])
#$(info int_div(100,7): [$(call int_div,100,7)])
# negative numbers
#$(info int_div(-5,1): [$(call int_div,-5,1)])
#$(info int_div(5,-2): [$(call int_div,5,-2)])
#$(info int_div(-123,-7): [$(call int_div,-123,-7)])


### combination tests

# (/ (- (+ 515 (* 222 311)) 300) 41) = 1689
#$(info int_mult,222,311: [$(call int_mult,222,311)])
#$(info int_add(515,69042): [$(call int_add,515,69042)])
#$(info int_sub(69557,300): [$(call int_sub,69557,300)])
#$(info int_div(69257,41): [$(call int_div,69257,41)])
# (/ (- (+ 515 (* -222 311)) 300) 41) = -1678
#$(info int_mult,-222,311: [$(call int_mult,-222,311)])
#$(info int_add(515,-69042): [$(call int_add,515,-69042)])
#$(info int_sub(-68527,300): [$(call int_sub,-68527,300)])
#$(info int_div(-68827,41): [$(call int_div,-68827,41)])

###############################################################

all:
	@true

endif

# vim: ts=2 et
