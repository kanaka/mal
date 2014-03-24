#
# mal (Make Lisp) trimmed and namespaced GMSL functions/definitions
# 	- derived from the GMSL 1.1.3
#

ifndef __mal_gmsl_included
__mal_gmsl_included := true

# ----------------------------------------------------------------------------
#
# GNU Make Standard Library (GMSL)
#
# A library of functions to be used with GNU Make's $(call) that
# provides functionality not available in standard GNU Make.
#
# Copyright (c) 2005-2013 John Graham-Cumming
#
# This file is part of GMSL
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#
# Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.
# 
# Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.
#
# Neither the name of the John Graham-Cumming nor the names of its
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
#
# ----------------------------------------------------------------------------


# Numbers
__gmsl_sixteen := x x x x x x x x x x x x x x x x
__gmsl_input_int := $(foreach a,$(__gmsl_sixteen),         \
                        $(foreach b,$(__gmsl_sixteen),     \
                            $(foreach c,$(__gmsl_sixteen), \
                                $(__gmsl_sixteen)))))

int_decode = $(words $1)
int_encode = $(wordlist 1,$1,$(__gmsl_input_int))

__gmsl_int_wrap = $(call int_decode,$(call $1,$(call int_encode,$2),$(call int_encode,$3)))

int_plus = $(strip $1 $2)
int_subtract = $(strip $(if $(call int_gte,$1,$2), \
                $(filter-out xx,$(join $1,$2)),    \
                $(warning Subtraction underflow)))
int_multiply = $(strip $(foreach a,$1,$2))
# _error function must be provided to report/catch division by zero
int_divide = $(strip $(if $2,                                              \
                 $(if $(call int_gte,$1,$2),                               \
                     x $(call int_divide,$(call int_subtract,$1,$2),$2),), \
                 $(call _error,Division by zero)))

int_max = $(subst xx,x,$(join $1,$2))
int_min = $(subst xx,x,$(filter xx,$(join $1,$2)))
int_gt = $(strip $(filter-out $(words $2),$(words $(call int_max,$1,$2))))
int_gte = $(strip $(call int_gt,$1,$2)$(call int_eq,$1,$2))
int_lt = $(strip $(filter-out $(words $1),$(words $(call int_max,$1,$2))))
int_lte = $(strip $(call int_lt,$1,$2)$(call int_eq,$1,$2))
int_eq = $(strip $(filter $(words $1),$(words $2)))
int_ne = $(strip $(filter-out $(words $1),$(words $2)))

gmsl_plus = $(call __gmsl_int_wrap,int_plus,$1,$2)
gmsl_subtract = $(call __gmsl_int_wrap,int_subtract,$1,$2)
gmsl_multiply = $(call __gmsl_int_wrap,int_multiply,$1,$2)
gmsl_divide = $(call __gmsl_int_wrap,int_divide,$1,$2)


# Strings

__gmsl_characters := A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
__gmsl_characters += a b c d e f g h i j k l m n o p q r s t u v w x y z
__gmsl_characters += 0 1 2 3 4 5 6 7 8 9
__gmsl_characters += ` ~ ! @ \# $$ % ^ & * ( ) - _ = +
__gmsl_characters += { } [ ] \ : ; ' " < > , . / ? |
__syntax_highlight_protect = #"'`


__gmsl_space := 
__gmsl_space +=

gmsl_strlen = $(strip $(eval __temp := $(subst $(__gmsl_space),x,$1)) \
                $(foreach a,$(__gmsl_characters),$(eval __temp := $$(subst $$a,x,$(__temp)))) \
                $(eval __temp := $(subst x,x ,$(__temp))) \
                $(words $(__temp)))

gmsl_merge = $(strip $(if $2, \
               $(if $(call _EQ,1,$(words $2)), \
                  $2,$(firstword $2)$1$(call gmsl_merge,$1,$(wordlist 2,$(words $2),$2)))))

gmsl_pairmap = $(strip \
                 $(if $2$3,$(call $1,$(word 1,$2),$(word 1,$3))     \
                           $(call gmsl_pairmap,$1,$(wordlist 2,$(words $2),$2),$(wordlist 2,$(words $3),$3))))

endif
