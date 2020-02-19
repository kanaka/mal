<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="3.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:fn="http://www.w3.org/2005/02/xpath-functions" xmlns:core="CORE" exclude-result-prefixes="core fn xsl">
    <xsl:function name="core:ns">
        <xsl:sequence>
            <malval kind="function" name="+" />
            <malval kind="function" name="-" />
            <malval kind="function" name="*" />
            <malval kind="function" name="/" />
            <malval kind="function" name="prn"/>
            <malval kind="function" name="pr-str"/>
            <malval kind="function" name="str"/>
            <malval kind="function" name="println"/>
            <malval kind="function" name="list"/>
            <malval kind="function" name="list?"/>
            <malval kind="function" name="empty?"/>
            <malval kind="function" name="count"/>
            <malval kind="function" name="="/>
            <malval kind="function" name="&lt;"/>
            <malval kind="function" name="&lt;="/>
            <malval kind="function" name="&gt;"/>
            <malval kind="function" name="&gt;="/>
            <malval kind="function" name="read-string"/>
            <malval kind="function" name="slurp"/>
            <malval kind="function" name="eval"/> <!-- defined in the step files -->
            <malval kind="function" name="atom"/> <!-- defined in the step files -->
            <malval kind="function" name="atom?"/>
            <malval kind="function" name="deref"/> <!-- defined in the step files -->
            <malval kind="function" name="swap!"/> <!-- defined in the step files -->
            <malval kind="function" name="reset!"/> <!-- defined in the step files -->
            <malval kind="function" name="cons"/>
            <malval kind="function" name="concat"/>
        </xsl:sequence>
    </xsl:function>

    <xsl:template name="core-apply">
      <xsl:param name="func" />
      <xsl:param name="args" />
      <xsl:choose>
        <xsl:when test="$func/malval/@kind = 'function'">
          <xsl:choose>
            <xsl:when test="$func/malval/@name = '+'">
              <xsl:variable name="result" select="number($args/value/malval/lvalue/malval[1]/@value) + number($args/value/malval/lvalue/malval[2]/@value)"></xsl:variable>
              <xsl:sequence select="core:makeMALType($result, 'number')"/>
            </xsl:when>
            <xsl:when test="$func/malval/@name = '-'">
              <xsl:variable name="result" select="number($args/value/malval/lvalue/malval[1]/@value) - number($args/value/malval/lvalue/malval[2]/@value)"></xsl:variable>
              <xsl:sequence select="core:makeMALType($result, 'number')"/>
            </xsl:when>
            <xsl:when test="$func/malval/@name = '*'">
              <xsl:variable name="result" select="number($args/value/malval/lvalue/malval[1]/@value) * number($args/value/malval/lvalue/malval[2]/@value)"></xsl:variable>
              <xsl:sequence select="core:makeMALType($result, 'number')"/>
            </xsl:when>
            <xsl:when test="$func/malval/@name = '/'">
              <xsl:variable name="result" select="number($args/value/malval/lvalue/malval[1]/@value) div number($args/value/malval/lvalue/malval[2]/@value)"></xsl:variable>
              <xsl:sequence select="core:makeMALType($result, 'number')"/>
            </xsl:when>
            <xsl:when test="$func/malval/@name = 'prn'">
                <xsl:variable name="args" select="$args/value/malval/lvalue/malval" />
                <xsl:variable name="sargs">
                  <xsl:for-each select="$args">
                    <xsl:variable name="arg">
                      <value>
                          <xsl:sequence select="."/>
                      </value>
                    </xsl:variable>
                    <str>
                        <xsl:for-each select="$arg">
                            <xsl:call-template name="malprinter-pr_str"><xsl:with-param name="readably" select="true()"/></xsl:call-template>
                        </xsl:for-each>
                    </str>
                  </xsl:for-each>
                </xsl:variable>
                <xsl:message>
                    <xsl:sequence select="string-join($sargs/str, ' ')"/>
                </xsl:message>
                <xsl:sequence select="core:makeMALType((), 'nil')"/>
            </xsl:when>
            <xsl:when test="$func/malval/@name = 'pr-str'">
                <xsl:variable name="args" select="$args/value/malval/lvalue/malval" />
                <xsl:variable name="sargs">
                  <xsl:for-each select="$args">
                    <xsl:variable name="arg">
                      <value>
                          <xsl:sequence select="."/>
                      </value>
                    </xsl:variable>
                    <str>
                        <xsl:for-each select="$arg">
                            <xsl:call-template name="malprinter-pr_str"><xsl:with-param name="readably" select="true()"/></xsl:call-template>
                        </xsl:for-each>
                    </str>
                  </xsl:for-each>
                </xsl:variable>
                <xsl:sequence select="core:makeMALType(string-join($sargs/str, ' '), 'string')"/>
            </xsl:when>
            <xsl:when test="$func/malval/@name = 'str'">
                <xsl:variable name="args" select="$args/value/malval/lvalue/malval" />
                <xsl:variable name="sargs">
                  <xsl:for-each select="$args">
                    <xsl:variable name="arg">
                      <value>
                          <xsl:sequence select="."/>
                      </value>
                    </xsl:variable>
                    <str>
                        <xsl:for-each select="$arg">
                            <xsl:call-template name="malprinter-pr_str"><xsl:with-param name="readably" select="false()"/></xsl:call-template>
                        </xsl:for-each>
                    </str>
                  </xsl:for-each>
                </xsl:variable>
                <xsl:sequence select="core:makeMALType(string-join($sargs/str, ''), 'string')"/>
            </xsl:when>
            <xsl:when test="$func/malval/@name = 'println'">
                <xsl:variable name="args" select="$args/value/malval/lvalue/malval" />
                <xsl:variable name="sargs">
                  <xsl:for-each select="$args">
                    <xsl:variable name="arg">
                      <value>
                          <xsl:sequence select="."/>
                      </value>
                    </xsl:variable>
                    <str>
                        <xsl:for-each select="$arg">
                            <xsl:call-template name="malprinter-pr_str"><xsl:with-param name="readably" select="false()"/></xsl:call-template>
                        </xsl:for-each>
                    </str>
                  </xsl:for-each>
                </xsl:variable>
                <xsl:message>
                    <xsl:sequence select="string-join($sargs/str, ' ')"/>
                </xsl:message>
                <xsl:sequence select="core:makeMALType((), 'nil')"/>
            </xsl:when>
            <xsl:when test="$func/malval/@name = 'list'">
                <xsl:sequence select="$args"/>
            </xsl:when>
            <xsl:when test="$func/malval/@name = 'list?'">
                <xsl:sequence select="core:makeMALType((), if ($args/value/malval/lvalue/malval[1]/@kind = 'list') then 'true' else 'false')"/>
            </xsl:when>
            <xsl:when test="$func/malval/@name = 'empty?'">
                <xsl:sequence select="core:makeMALType((), if (count($args/value/malval/lvalue/malval[1]/lvalue/malval) = 0) then 'true' else 'false')"/>
            </xsl:when>
            <xsl:when test="$func/malval/@name = 'count'">
                <xsl:sequence select="core:makeMALType(count($args/value/malval/lvalue/malval[1]/lvalue/malval), 'number')"/>
            </xsl:when>
            <xsl:when test="$func/malval/@name = '='">
                <xsl:sequence select="core:makeMALType((), if (core:equal($args/value/malval/lvalue/malval[1], $args/value/malval/lvalue/malval[2])) then 'true' else 'false')"/>
            </xsl:when>
            <xsl:when test="$func/malval/@name = '&lt;'">
                <xsl:sequence select="core:makeMALType((), if (number($args/value/malval/lvalue/malval[1]/@value) lt number($args/value/malval/lvalue/malval[2]/@value)) then 'true' else 'false')"/>
            </xsl:when>
            <xsl:when test="$func/malval/@name = '&lt;='">
                <xsl:sequence select="core:makeMALType((), if (number($args/value/malval/lvalue/malval[1]/@value) le number($args/value/malval/lvalue/malval[2]/@value)) then 'true' else 'false')"/>
            </xsl:when>
            <xsl:when test="$func/malval/@name = '&gt;'">
                <xsl:sequence select="core:makeMALType((), if (number($args/value/malval/lvalue/malval[1]/@value) gt number($args/value/malval/lvalue/malval[2]/@value)) then 'true' else 'false')"/>
            </xsl:when>
            <xsl:when test="$func/malval/@name = '&gt;='">
                <xsl:sequence select="core:makeMALType((), if (number($args/value/malval/lvalue/malval[1]/@value) ge number($args/value/malval/lvalue/malval[2]/@value)) then 'true' else 'false')"/>
            </xsl:when>
            <xsl:when test="$func/malval/@name = 'read-string'">
              <xsl:variable name="read-string-context">
                <str>
                  <xsl:value-of select="$args/value/malval/lvalue/malval[1]/@value"></xsl:value-of>
                </str>
              </xsl:variable>
              <xsl:for-each select="$read-string-context">
                <xsl:call-template name="malreader-read_str"></xsl:call-template>
              </xsl:for-each>
            </xsl:when>
            <xsl:when test="$func/malval/@name = 'slurp'">
                <xsl:sequence select="core:makeMALType(unparsed-text($args/value/malval/lvalue/malval[1]/@value), 'string')"/>
            </xsl:when>
            <xsl:when test="$func/malval/@name = 'atom?'">
                <xsl:sequence select="core:makeMALType((), if ($args/value/malval/lvalue/malval[1]/@kind = 'atom') then 'true' else 'false')"/>
            </xsl:when>
            <xsl:when test="$func/malval/@name = 'cons'">
              <xsl:variable name="result">
                <value>
                  <malval kind="list">
                    <lvalue>
                      <xsl:sequence select="$args/value/malval/lvalue/malval[1]"/>
                      <xsl:sequence select="$args/value/malval/lvalue/malval[2]/lvalue/malval" />
                    </lvalue>
                  </malval>
                </value>
              </xsl:variable>
              <xsl:sequence select="$result"/>
            </xsl:when>
            <xsl:when test="$func/malval/@name = 'concat'">
              <xsl:variable name="result">
                <value>
                  <malval kind="list">
                    <lvalue>
                      <xsl:sequence select="$args/value/malval/lvalue/malval/lvalue/malval"/>
                    </lvalue>
                  </malval>
                </value>
              </xsl:variable>
              <xsl:sequence select="$result"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:message terminate="yes">Invalid function <xsl:sequence select="$func"/> </xsl:message>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:when>
        <xsl:otherwise></xsl:otherwise>
      </xsl:choose>
    </xsl:template>

    <xsl:function name="core:makeMALType">
      <xsl:param name="value" />
      <xsl:param name="kind" />
      <value>
        <malval kind="{$kind}" value="{$value}"/>
      </value>
    </xsl:function>

    <xsl:function name="core:all-equal">
        <xsl:param name="seq"/>
        <xsl:param name="left"/>
        <xsl:param name="right"/>
        <xsl:choose>
            <xsl:when test="empty($seq)">
              <xsl:sequence select="true()"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:choose>
                  <xsl:when test="core:list-equal($left, $right, head($seq))">
                    <xsl:sequence select="core:all-equal(tail($seq), $right, $left)"/>
                  </xsl:when>
                  <xsl:otherwise>
                    <xsl:sequence select="false()"/>
                  </xsl:otherwise>
              </xsl:choose>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:function>

    <xsl:function name="core:equal">
        <xsl:param name="left"/>
        <xsl:param name="right"/>
        <xsl:choose>
            <!-- equal kinds -->
            <xsl:when test="$left/@kind = $right/@kind">
              <xsl:choose>
                <!-- sequence? -->
                <xsl:when test="$left/@kind = 'list' or $left/@kind = 'vector' or $left/@kind = 'hash'">
                  <xsl:choose>
                    <!-- same counts -->
                    <xsl:when test="count($left/lvalue/malval) = count($right/lvalue/malval)">
                      <xsl:sequence select="core:all-equal(1 to count($left/lvalue/malval), $left, $right)"/>
                    </xsl:when>
                    <!-- different counts -->
                    <xsl:otherwise>
                        <xsl:sequence select="false()"/>
                    </xsl:otherwise>
                  </xsl:choose>
                </xsl:when>
                <!-- simple 'value' type -->
                <xsl:otherwise>
                    <xsl:sequence select="string($left/@value) = string($right/@value)"/>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:when>
            <!-- different types -->
            <xsl:otherwise>
                <xsl:choose>
                  <xsl:when test="($left/@kind = 'list' and $right/@kind = 'vector') or ($left/@kind = 'vector' and $right/@kind = 'list')">
                    <xsl:choose>
                      <!-- same counts -->
                      <xsl:when test="count($left/lvalue/malval) = count($right/lvalue/malval)">
                        <xsl:sequence select="core:all-equal(1 to count($left/lvalue/malval), $left, $right)"/>
                      </xsl:when>
                      <!-- different counts -->
                      <xsl:otherwise>
                          <xsl:sequence select="false()"/>
                      </xsl:otherwise>
                    </xsl:choose>
                  </xsl:when>
                  <xsl:otherwise>
                    <xsl:sequence select="false()"/>
                  </xsl:otherwise>
                </xsl:choose>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:function>

    <xsl:function name="core:list-equal">
        <xsl:param name="l"/>
        <xsl:param name="r"/>
        <xsl:param name="v"/>

        <xsl:sequence select="core:equal($l/lvalue/malval[$v], $r/lvalue/malval[$v])"/>
    </xsl:function>
</xsl:stylesheet>