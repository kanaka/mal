from typing import Dict

from arpeggio import (  # type: ignore
    ParserPython,
    PTNodeVisitor,
    visit_parse_tree,
    ZeroOrMore,
)
from arpeggio import RegExMatch as _, NoMatch  # type: ignore

from mal_types import (
    MalExpression,
    MalInt,
    MalList,
    MalBoolean,
    MalNil,
    MalVector,
    MalHash_map,
)
from mal_types import MalSymbol, MalString, MalSyntaxException


# Arpeggio grammar
def mExpression():
    return [
        mQuotedExpression,
        mQuasiQuotedExpression,
        mSpliceUnquotedExpression,
        mUnquotedExpression,
        mDerefExpression,
        mList,
        mVector,
        mHash_map,
        mInt,
        mString,
        mKeyword,
        mNil,
        mBoolean,
        mSymbol,
    ]


def mQuotedExpression():
    return "'", mExpression


def mQuasiQuotedExpression():
    return "`", mExpression


def mSpliceUnquotedExpression():
    return "~@", mExpression


def mUnquotedExpression():
    return "~", mExpression


def mDerefExpression():
    return "@", mExpression


def mList():
    return "(", ZeroOrMore(mExpression), ")"


def mVector():
    return "[", ZeroOrMore(mExpression), "]"


def mHash_map():
    return ("{", ZeroOrMore(mExpression), "}")


def mInt():
    return _(r"-?[0123456789]+")


def mString():
    return _(r""""(?:\\.|[^\\"])*"?""")


def mKeyword():
    return _(r""":[^\s\[\]{}('"`,;)]*""")


def mSymbol():
    return _(r"""[^\s\[\]{}('"`,;)]*""")


def mNil():
    return _(r"""nil(?!\?)""")


def mBoolean():
    return _(r"""(true|false)(?!\?)""")


class ReadASTVisitor(PTNodeVisitor):
    def visit_mExpression(self, node, children) -> MalExpression:
        return children[0]  # children should already be Mal types

    def visit_mInt(self, node, children) -> MalInt:
        return MalInt(int(node.value))

    def visit_mString(self, node, children) -> MalString:
        # node.value will have quotes, escape sequences
        assert type(node.value) is str
        if node.value[0] != '"':
            raise Exception("internal error: parsed a string with no start quote")
        val: str = node.value
        if len(val) < 2 or val[-1] != '"':
            raise MalSyntaxException("unbalanced string")
        val = val[1:-1]  # remove outer quotes

        # handle escaped characters
        i = 0
        result = ""
        while i < len(val):
            if val[i] == "\\":
                if (i + 1) < len(val):
                    if val[i + 1] == "n":
                        result += "\n"
                    elif val[i + 1] == "\\":
                        result += "\\"
                    elif val[i + 1] == '"':
                        result += '"'
                    i += 2
                else:
                    raise MalSyntaxException(
                        "unbalanced string or invalid escape sequence"
                    )
            else:
                result += val[i]
                i += 1

        return MalString(result)

    def visit_mKeyword(self, node, children) -> MalString:
        assert type(node.value) is str
        assert len(node.value) > 1
        return MalString(node.value[1:], keyword=True)

    def visit_mList(self, node, children) -> MalList:
        return MalList(children)

    def visit_mVector(self, node, children) -> MalVector:
        return MalVector(children)

    def visit_mHash_map(self, node, children):
        assert len(children) % 2 == 0
        dict = {}  # type: Dict[MalExpression, MalExpression]
        for i in range(0, len(children), 2):
            assert isinstance(children[i], MalString)
            dict[children[i].native()] = children[i + 1]
        return MalHash_map(dict)

    def visit_mSymbol(self, node, children) -> MalSymbol:
        return MalSymbol(node.value)

    def visit_mBoolean(self, node, children) -> MalBoolean:
        if node.value == "true":
            return MalBoolean(True)
        if node.value == "false":
            return MalBoolean(False)
        raise Exception("Internal reader error")

    def visit_mNil(self, node, children) -> MalNil:
        return MalNil()

    def visit_mQuotedExpression(self, node, children) -> MalList:
        return MalList([MalSymbol("quote"), children[0]])

    def visit_mQuasiQuotedExpression(self, node, children) -> MalList:
        return MalList([MalSymbol("quasiquote"), children[0]])

    def visit_mSpliceUnquotedExpression(self, node, children) -> MalList:
        return MalList([MalSymbol("splice-unquote"), children[0]])

    def visit_mUnquotedExpression(self, node, children) -> MalList:
        return MalList([MalSymbol("unquote"), children[0]])

    def visit_mDerefExpression(self, node, children) -> MalList:
        return MalList([MalSymbol("deref"), children[0]])


def comment():
    return _(";.*")


def read(x: str) -> MalExpression:
    """Parse a string into a MalExpression"""
    reader = ParserPython(mExpression, comment_def=comment, ws="\t\n\r ,", debug=False)

    try:
        parsed = visit_parse_tree(reader.parse(x), ReadASTVisitor())
        assert issubclass(type(parsed), MalExpression)
        return parsed
    except NoMatch as e:
        # print(str(e))
        raise MalSyntaxException("invalid syntax or unexpected EOF")
