<?xml version="1.0" encoding="UTF-8"?>
<!-- Step 3: Environment -->
<!-- input document must be in the following format -->
<!--
<mal>
    <stdin>...stdin text...</stdin>
    <stdout> ... ignored, omitted ... </stdout>
    <state> contains env </state>
</mal>
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:fn="http://www.w3.org/2005/02/xpath-functions" xmlns:env="ENV" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:map="http://www.w3.org/2005/xpath-functions/map" version="3.0" exclude-result-prefixes="fn xs map env">
  <xsl:import href="reader.xslt"/>
  <xsl:import href="printer.xslt"/>
  <xsl:import href="env.xslt"/>
  <xsl:output method="xml" encoding="utf-8" indent="yes"/>
  <xsl:template match="mal" name="rep">
    <xsl:param name="display" select="false()" />
    <mal>
      <xsl:variable name="env" as="map(*)">
        <xsl:sequence select="env:deserialise((state/env/@data, env:base())[1])"/>
      </xsl:variable>
      <xsl:sequence select="stdin"/>
      <xsl:variable name="_read">
        <xsl:call-template name="READ"/>
      </xsl:variable>
      <xsl:variable name="_eval">
        <xsl:for-each select="$_read">
          <xsl:call-template name="EVAL">
            <xsl:with-param name="env" select="$env"/>
          </xsl:call-template>
        </xsl:for-each>
      </xsl:variable>
      <xsl:for-each select="$_eval">
        <xsl:variable name="_print">
          <xsl:for-each select="data">
            <xsl:call-template name="PRINT"/>
          </xsl:for-each>
        </xsl:variable>
        <xsl:message>
          <request kind="display" value="{$_print}"/>
        </xsl:message>
        <state>
          <env data="{env/@data}"/>
        </state>
      </xsl:for-each>
    </mal>
  </xsl:template>
  <xsl:template name="PRINT">
    <xsl:variable name="str">
      <xsl:call-template name="malprinter-pr_str">
        <xsl:with-param name="readably" select="true()"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:value-of select="$str"/>
  </xsl:template>
  <xsl:template name="eval_ast">
    <xsl:param name="env"/>
    <xsl:choose>
      <xsl:when test="value/malval/@kind = 'symbol'">
        <xsl:variable name="val">
          <xsl:sequence select="env:get($env, value/malval/@value)"/>
        </xsl:variable>
        <value>
          <xsl:sequence select="$val"/>
        </value>
      </xsl:when>
      <xsl:when test="value/malval/@kind = 'list'">
        <value>
          <malval kind="list">
            <lvalue>
              <xsl:for-each select="value/malval/lvalue/malval">
                <xsl:variable name="ctx">
                  <value>
                    <xsl:sequence select="."/>
                  </value>
                </xsl:variable>
                <xsl:variable name="xctx">
                  <xsl:for-each select="$ctx">
                    <xsl:variable name="val">
                      <xsl:call-template name="EVAL">
                        <xsl:with-param name="env" select="$env"/>
                        <xsl:with-param name="encode-env" select="false()"/>
                      </xsl:call-template>
                    </xsl:variable>
                    <xsl:sequence select="$val/data/value"/>
                  </xsl:for-each>
                </xsl:variable>
                <xsl:sequence select="$xctx/value/malval"/>
              </xsl:for-each>
            </lvalue>
          </malval>
        </value>
      </xsl:when>
      <xsl:when test="value/malval/@kind = 'vector'">
        <value>
          <malval kind="vector">
            <lvalue>
              <xsl:for-each select="value/malval/lvalue/malval">
                <xsl:variable name="ctx">
                  <value>
                    <xsl:sequence select="."/>
                  </value>
                </xsl:variable>
                <xsl:variable name="xctx">
                  <xsl:for-each select="$ctx">
                    <xsl:variable name="val">
                      <xsl:call-template name="EVAL">
                        <xsl:with-param name="env" select="$env"/>
                        <xsl:with-param name="encode-env" select="false()"/>
                      </xsl:call-template>
                    </xsl:variable>
                    <xsl:sequence select="$val/data/value"/>
                  </xsl:for-each>
                </xsl:variable>
                <xsl:sequence select="$xctx/value/malval"/>
              </xsl:for-each>
            </lvalue>
          </malval>
        </value>
      </xsl:when>
      <xsl:when test="value/malval/@kind = 'hash'">
        <value>
          <malval kind="hash">
            <lvalue>
              <xsl:for-each select="value/malval/lvalue/malval">
                <xsl:variable name="ctx">
                  <value>
                    <xsl:sequence select="."/>
                  </value>
                </xsl:variable>
                <xsl:variable name="xctx">
                  <xsl:for-each select="$ctx">
                    <xsl:variable name="val">
                      <xsl:call-template name="EVAL">
                        <xsl:with-param name="env" select="$env"/>
                        <xsl:with-param name="encode-env" select="false()"/>
                      </xsl:call-template>
                    </xsl:variable>
                    <xsl:sequence select="$val/data/value"/>
                  </xsl:for-each>
                </xsl:variable>
                <xsl:sequence select="$xctx/value/malval"/>
              </xsl:for-each>
            </lvalue>
          </malval>
        </value>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="."/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <!-- vapply[fn, args] :: fn/value/text() -->
  <xsl:template name="vapply">
    <xsl:param name="func"/>
    <xsl:param name="args"/>
    <xsl:choose>
      <xsl:when test="$func/malval/@kind = 'function'">
        <xsl:choose>
          <xsl:when test="$func/malval/@name = '+'">
            <xsl:variable name="result" select="number($args/value/malval/lvalue/malval[1]/@value) + number($args/value/malval/lvalue/malval[2]/@value)"/>
            <xsl:sequence select="fn:makeMALType($result, 'number')"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = '-'">
            <xsl:variable name="result" select="number($args/value/malval/lvalue/malval[1]/@value) - number($args/value/malval/lvalue/malval[2]/@value)"/>
            <xsl:sequence select="fn:makeMALType($result, 'number')"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = '*'">
            <xsl:variable name="result" select="number($args/value/malval/lvalue/malval[1]/@value) * number($args/value/malval/lvalue/malval[2]/@value)"/>
            <xsl:sequence select="fn:makeMALType($result, 'number')"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = '/'">
            <xsl:variable name="result" select="number($args/value/malval/lvalue/malval[1]/@value) div number($args/value/malval/lvalue/malval[2]/@value)"/>
            <xsl:sequence select="fn:makeMALType($result, 'number')"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="error(QName('MAL', 'Error'), concat('Invalid function ', $func))"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:otherwise/>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="EVAL">
    <xsl:param name="env"/>
    <xsl:param name="encode-env" select="true()"/>
    <xsl:variable name="data">
      <xsl:choose>
        <xsl:when test="value/malval/@kind = 'list'">
          <xsl:choose>
            <xsl:when test="count(value/malval/lvalue/malval) = 0">
              <xsl:sequence select="."/>
              <xsl:if test="$encode-env">
                <env data="{env:serialise($env)}"/>
              </xsl:if>
            </xsl:when>
            <xsl:otherwise>
              <xsl:choose>
                  <xsl:when test="let $fn := value/malval/lvalue/malval[1]
                                      return $fn/@kind = 'symbol' and
                                           $fn/@value = 'def!'">
                  <xsl:variable name="name">
                    <xsl:value-of select="value/malval/lvalue/malval[2]/@value"/>
                  </xsl:variable>
                  <xsl:variable name="xvalue">
                    <value>
                      <xsl:sequence select="value/malval/lvalue/malval[3]"/>
                    </value>
                  </xsl:variable>
                  <xsl:variable name="value">
                    <xsl:for-each select="$xvalue">
                      <xsl:call-template name="EVAL">
                        <xsl:with-param name="env" select="$env"/>
                        <xsl:with-param name="encode-env" select="false()"/>
                      </xsl:call-template>
                    </xsl:for-each>
                  </xsl:variable>
                  <xsl:sequence select="$value/data/value"/>
                  <xsl:if test="$encode-env">
                    <env data="{env:serialise(env:set($env, $name, $value/data/value/malval))}"/>
                  </xsl:if>
                </xsl:when>
                <xsl:when test="let $fn := value/malval/lvalue/malval[1]
                                    return $fn/@kind = 'symbol' and
                                           $fn/@value = 'let*'">
                  <xsl:variable name="xvalue">
                    <value>
                      <xsl:sequence select="value/malval/lvalue/malval[3]"/>
                    </value>
                  </xsl:variable>
                  <xsl:variable name="new_env" select="env:close($env)"/>
                  <xsl:iterate select="fn:group_consec(value/malval/lvalue/malval[2]/lvalue/malval)">
                    <xsl:param name="new_env" select="$env"/>
                    <xsl:on-completion>
                      <xsl:variable name="value">
                        <xsl:for-each select="$xvalue">
                          <xsl:call-template name="EVAL">
                            <xsl:with-param name="env" select="$new_env"/>
                            <xsl:with-param name="encode-env" select="false()"/>
                          </xsl:call-template>
                        </xsl:for-each>
                      </xsl:variable>
                      <xsl:sequence select="$value/data/value"/>
                      <xsl:if test="$encode-env">
                        <env data="{env:serialise($env)}"/>
                      </xsl:if>
                    </xsl:on-completion>
                    <xsl:variable name="name">
                      <xsl:value-of select="node()[name() = 'first']/malval/@value"/>
                    </xsl:variable>
                    <xsl:variable name="xvalue">
                      <value>
                        <xsl:sequence select="node()[name() = 'second']/malval"/>
                      </value>
                    </xsl:variable>
                    <xsl:variable name="value">
                      <xsl:for-each select="$xvalue">
                        <xsl:call-template name="EVAL">
                          <xsl:with-param name="env" select="$new_env"/>
                          <xsl:with-param name="encode-env" select="false()"/>
                        </xsl:call-template>
                      </xsl:for-each>
                    </xsl:variable>
                    <xsl:next-iteration>
                      <xsl:with-param name="new_env" select="env:set($new_env, $name, $value/data/value/malval)"/>
                    </xsl:next-iteration>
                  </xsl:iterate>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:variable name="new_list">
                    <xsl:call-template name="eval_ast">
                      <xsl:with-param name="env" select="$env"/>
                    </xsl:call-template>
                  </xsl:variable>
                  <xsl:variable name="func">
                    <xsl:for-each select="$new_list">
                      <xsl:sequence select="value/malval/lvalue/malval[1]"/>
                    </xsl:for-each>
                  </xsl:variable>
                  <xsl:variable name="args">
                    <xsl:for-each select="$new_list">
                      <value>
                        <malval kind="list">
                          <lvalue>
                            <xsl:for-each select="value/malval/lvalue/node()[position() != 1]">
                              <xsl:sequence select="."/>
                            </xsl:for-each>
                          </lvalue>
                        </malval>
                      </value>
                    </xsl:for-each>
                  </xsl:variable>
                  <xsl:call-template name="vapply">
                    <xsl:with-param name="func" select="$func"/>
                    <xsl:with-param name="args" select="$args"/>
                  </xsl:call-template>
                  <xsl:if test="$encode-env">
                    <env data="{env:serialise($env)}"/>
                  </xsl:if>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:when>
        <xsl:otherwise>
          <xsl:call-template name="eval_ast">
            <xsl:with-param name="env" select="$env"/>
          </xsl:call-template>
          <xsl:if test="$encode-env">
            <env data="{env:serialise($env)}"/>
          </xsl:if>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <data>
      <xsl:sequence select="$data/value"/>
    </data>
    <xsl:if test="$encode-env">
      <env data="{$data/env/@data}"/>
    </xsl:if>
  </xsl:template>
  <xsl:template name="READ">
    <xsl:variable name="context">
      <str>
        <xsl:copy-of select="stdin/text()"/>
      </str>
    </xsl:variable>
    <xsl:variable name="form">
      <xsl:for-each select="$context">
        <xsl:call-template name="malreader-read_str"/>
      </xsl:for-each>
    </xsl:variable>
    <xsl:for-each select="$form">
      <xsl:if test="error">
        <xsl:value-of select="error(QName('MAL', 'Error'), string(error))"/>
      </xsl:if>
      <xsl:copy-of select="."/>
    </xsl:for-each>
  </xsl:template>
  <xsl:function name="fn:makeMALType">
    <xsl:param name="value"/>
    <xsl:param name="kind"/>
    <value>
      <malval kind="{$kind}" value="{$value}"/>
    </value>
  </xsl:function>
  <xsl:function name="fn:group_consec">
    <xsl:param name="nodes"/>
    <xsl:variable name="groups">
      <xsl:for-each-group select="$nodes" group-by="position() mod 2">
        <xsl:choose>
          <xsl:when test="position() = 1">
            <first>
              <xsl:sequence select="current-group()"/>
            </first>
          </xsl:when>
          <xsl:otherwise>
            <second>
              <xsl:sequence select="current-group()"/>
            </second>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:for-each-group>
    </xsl:variable>
    <xsl:iterate select="1 to count($groups/first/*)">
      <element>
        <xsl:variable name="idx" select="number(.)"/>
        <first>
          <xsl:sequence select="$groups/first/node()[position() = $idx]"/>
        </first>
        <second>
          <xsl:sequence select="$groups/second/node()[position() = $idx]"/>
        </second>
      </element>
    </xsl:iterate>
  </xsl:function>
</xsl:stylesheet>
