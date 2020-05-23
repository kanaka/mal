<?xml version="1.0" encoding="UTF-8"?>
<!-- Step 4: If Fn Do -->
<!-- input document must be in the following format -->
<!--
<mal>
    <stdin>...stdin text...</stdin>
    <stdout> ... ignored, omitted ... </stdout>
    <state> contains env </state>
</mal>
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:fn="http://www.w3.org/2005/02/xpath-functions" xmlns:env="ENV" xmlns:core="CORE" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:map="http://www.w3.org/2005/xpath-functions/map" version="3.0" exclude-result-prefixes="fn xs map env core">
  <xsl:import href="reader.xslt"/>
  <xsl:import href="printer.xslt"/>
  <xsl:import href="env.xslt"/>
  <xsl:import href="core.xslt"/>
  <xsl:output method="xml" encoding="utf-8" indent="yes"/>
  <xsl:template match="mal" name="rep">
    <xsl:param name="display" select="false()" />
    <xsl:choose>
      <xsl:when test="string(state/env/@data) = ''">
        <xsl:variable name="vstate">
          <mal>
            <state>
              <env data="{env:serialise(env:empty() =&gt; env:bind-all(core:ns()/@name, core:ns()))}"/>
            </state>
            <stdin>(def! not (fn* (a) (if a false true)))</stdin>
          </mal>
        </xsl:variable>
        <xsl:variable name="new-state">
          <xsl:for-each select="$vstate/mal">
            <xsl:call-template name="rep"/>
          </xsl:for-each>
        </xsl:variable>
        <xsl:variable name="state-v">
          <xsl:sequence select="$new-state/mal/state"/>
          <xsl:sequence select="stdin"/>
        </xsl:variable>
        <xsl:for-each select="$state-v">
          <xsl:call-template name="rep">
            <xsl:with-param name="display" select="$display"/>
          </xsl:call-template>
        </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
        <mal>
          <xsl:variable name="env" as="map(*)">
            <xsl:sequence select="env:deserialise(state/env/@data)"/>
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
            <xsl:if test="$display">
              <xsl:message>
                <request kind="display" value="{$_print}"/>
              </xsl:message>
            </xsl:if>
            <state>
              <env data="{env/@data}"/>
            </state>
          </xsl:for-each>
        </mal>
      </xsl:otherwise>
    </xsl:choose>
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
  <!-- uapply[env, fn, args] -->
  <xsl:template name="uapply">
    <xsl:param name="func"/>
    <xsl:param name="args"/>
    <xsl:param name="env"/>
    <xsl:variable name="nenv" select="$env =&gt; env:hier(env:deserialise($func/malval/env/@data)) =&gt; env:close-with-binds($func/malval/binds/malval/@value, $args/value/malval/lvalue/malval)"/>
    <xsl:variable name="body">
      <value>
        <xsl:sequence select="$func/malval/body/malval"/>
      </value>
    </xsl:variable>
    <xsl:variable name="result">
      <xsl:for-each select="$body">
        <xsl:call-template name="EVAL">
          <xsl:with-param name="env" select="$nenv"/>
          <xsl:with-param name="encode-env" select="false()"/>
        </xsl:call-template>
      </xsl:for-each>
    </xsl:variable>
    <xsl:sequence select="$result/data/value"/>
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
                          <xsl:with-param name="encode-env" select="$encode-env"/>
                        </xsl:call-template>
                      </xsl:for-each>
                    </xsl:variable>
                    <xsl:next-iteration>
                      <xsl:with-param name="new_env" select="env:set($new_env, $name, $value/data/value/malval)"/>
                    </xsl:next-iteration>
                  </xsl:iterate>
                </xsl:when>
                <xsl:when test="let $fn := value/malval/lvalue/malval[1]
                                    return $fn/@kind = 'symbol' and
                                           $fn/@value = 'do'">
                  <xsl:iterate select="value/malval/lvalue/malval[position() &gt; 1]">
                    <xsl:param name="new_env" select="$env"/>
                    <xsl:param name="previous_res" select="()"/>
                    <xsl:on-completion>
                      <xsl:sequence select="$previous_res"/>
                      <xsl:if test="$encode-env">
                        <env data="{env:serialise($new_env)}"/>
                      </xsl:if>
                    </xsl:on-completion>
                    <xsl:variable name="xvalue">
                      <value>
                        <xsl:sequence select="."/>
                      </value>
                    </xsl:variable>
                    <xsl:variable name="value">
                      <xsl:for-each select="$xvalue">
                        <xsl:call-template name="EVAL">
                          <xsl:with-param name="env" select="$new_env"/>
                          <xsl:with-param name="encode-env" select="true()"/>
                        </xsl:call-template>
                      </xsl:for-each>
                    </xsl:variable>
                    <xsl:next-iteration>
                      <xsl:with-param name="new_env" select="env:deserialise($value/env/@data)"/>
                      <xsl:with-param name="previous_res" select="$value/data/value"/>
                    </xsl:next-iteration>
                  </xsl:iterate>
                </xsl:when>
                <xsl:when test="let $fn := value/malval/lvalue/malval[1]
                                    return $fn/@kind = 'symbol' and
                                           $fn/@value = 'if'">
                  <xsl:variable name="cond">
                    <xsl:for-each select="value/malval/lvalue/malval[2]">
                      <xsl:variable name="context">
                        <value>
                          <xsl:sequence select="."/>
                        </value>
                      </xsl:variable>
                      <xsl:for-each select="$context">
                        <xsl:call-template name="EVAL">
                          <xsl:with-param name="env" select="$env"/>
                          <xsl:with-param name="encode-env" select="false()"/>
                        </xsl:call-template>
                      </xsl:for-each>
                    </xsl:for-each>
                  </xsl:variable>
                  <xsl:variable name="ptrue">
                    <xsl:for-each select="value/malval/lvalue/malval[3]">
                      <value>
                        <xsl:sequence select="."/>
                      </value>
                    </xsl:for-each>
                  </xsl:variable>
                  <xsl:variable name="pfalse">
                    <xsl:for-each select="value/malval/lvalue/malval[4]">
                      <value>
                        <xsl:sequence select="."/>
                      </value>
                    </xsl:for-each>
                  </xsl:variable>
                  <xsl:variable name="xfalse">
                    <xsl:choose>
                      <xsl:when test="empty($pfalse/value)">
                        <value>
                          <malval kind="nil"/>
                        </value>
                      </xsl:when>
                      <xsl:otherwise>
                        <xsl:sequence select="$pfalse/value"/>
                      </xsl:otherwise>
                    </xsl:choose>
                  </xsl:variable>
                  <xsl:variable name="res">
                    <xsl:choose>
                        <xsl:when test="let $kind := $cond/data/value/malval/@kind
                                            return $kind = 'nil' or $kind
                                               = 'false'">
                        <xsl:for-each select="$xfalse">
                          <xsl:call-template name="EVAL">
                            <xsl:with-param name="env" select="$env"/>
                            <xsl:with-param name="encode-env" select="$encode-env"/>
                          </xsl:call-template>
                        </xsl:for-each>
                      </xsl:when>
                      <xsl:otherwise>
                        <xsl:for-each select="$ptrue">
                          <xsl:call-template name="EVAL">
                            <xsl:with-param name="env" select="$env"/>
                            <xsl:with-param name="encode-env" select="$encode-env"/>
                          </xsl:call-template>
                        </xsl:for-each>
                      </xsl:otherwise>
                    </xsl:choose>
                  </xsl:variable>
                  <xsl:sequence select="$res/data/value"/>
                  <xsl:if test="$encode-env">
                    <env data="{env:serialise($env)}"/>
                  </xsl:if>
                </xsl:when>
                <xsl:when test="let $fn := value/malval/lvalue/malval[1]
                                    return $fn/@kind = 'symbol' and
                                           $fn/@value = 'fn*'">
                  <value>
                    <malval kind="userfunction">
                      <binds>
                        <xsl:sequence select="value/malval/lvalue/malval[2]/lvalue/malval"/>
                      </binds>
                      <body>
                        <xsl:sequence select="value/malval/lvalue/malval[3]"/>
                      </body>
                      <env data="{env:serialise($env)}"/>
                      <!-- capture current env -->
                    </malval>
                  </value>
                  <xsl:if test="$encode-env">
                    <env data="{env:serialise($env)}"/>
                  </xsl:if>
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
                  <xsl:choose>
                    <xsl:when test="$func/malval/@kind = 'userfunction'">
                      <xsl:call-template name="uapply">
                        <xsl:with-param name="env" select="$env"/>
                        <xsl:with-param name="func" select="$func"/>
                        <xsl:with-param name="args" select="$args"/>
                      </xsl:call-template>
                    </xsl:when>
                    <xsl:otherwise>
                      <xsl:call-template name="core-apply">
                        <xsl:with-param name="func" select="$func"/>
                        <xsl:with-param name="args" select="$args"/>
                      </xsl:call-template>
                    </xsl:otherwise>
                  </xsl:choose>
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
