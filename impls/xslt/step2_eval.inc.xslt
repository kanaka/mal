<?xml version="1.0" encoding="UTF-8"?>
<!-- Step 2: Eval -->
<!-- input document must be in the following format -->
<!--
<mal>
    <stdin>...stdin text...</stdin>
    <stdout> ... ignored, omitted ... </stdout>
    <state> ignored, preserved </state>
</mal>
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:fn="http://www.w3.org/2005/02/xpath-functions" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:map="http://www.w3.org/2005/xpath-functions/map" version="3.0" exclude-result-prefixes="fn xs map">
  <xsl:import href="reader.xslt"/>
  <xsl:import href="printer.xslt"/>
  <xsl:output method="xml" encoding="utf-8" indent="yes"/>
  <xsl:template match="mal" name="rep">
    <xsl:param name="display" select="false()" />
    <mal>
      <xsl:variable name="env" as="map(*)">
        <xsl:if test="not(state)">
          <!-- we never update this -->
          <xsl:map>
            <xsl:map-entry key="'+'" select="'+'"/>
            <xsl:map-entry key="'-'" select="'-'"/>
            <xsl:map-entry key="'*'" select="'*'"/>
            <xsl:map-entry key="'/'" select="'/'"/>
          </xsl:map>
        </xsl:if>
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
      <xsl:variable name="_print">
        <xsl:for-each select="$_eval">
          <xsl:call-template name="PRINT">
            <xsl:with-param name="env" select="$env"/>
          </xsl:call-template>
        </xsl:for-each>
      </xsl:variable>
      <xsl:message>
        <request kind="display" value="{$_print}"/>
      </xsl:message>
    </mal>
  </xsl:template>
  <xsl:template name="PRINT">
    <xsl:param name="env"/>
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
        <value>
          <xsl:sequence select="fn:env_lookup($env, value/malval/@value)"/>
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
                    <xsl:call-template name="EVAL">
                      <xsl:with-param name="env" select="$env"/>
                    </xsl:call-template>
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
                    <xsl:call-template name="EVAL">
                      <xsl:with-param name="env" select="$env"/>
                    </xsl:call-template>
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
                    <xsl:call-template name="EVAL">
                      <xsl:with-param name="env" select="$env"/>
                    </xsl:call-template>
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
      <xsl:when test="$func = '+'">
        <xsl:variable name="result" select="number($args/value/malval/lvalue/malval[1]/@value) + number($args/value/malval/lvalue/malval[2]/@value)"/>
        <xsl:sequence select="fn:makeMALType($result, 'number')"/>
      </xsl:when>
      <xsl:when test="$func = '-'">
        <xsl:variable name="result" select="number($args/value/malval/lvalue/malval[1]/@value) - number($args/value/malval/lvalue/malval[2]/@value)"/>
        <xsl:sequence select="fn:makeMALType($result, 'number')"/>
      </xsl:when>
      <xsl:when test="$func = '*'">
        <xsl:variable name="result" select="number($args/value/malval/lvalue/malval[1]/@value) * number($args/value/malval/lvalue/malval[2]/@value)"/>
        <xsl:sequence select="fn:makeMALType($result, 'number')"/>
      </xsl:when>
      <xsl:when test="$func = '/'">
        <xsl:variable name="result" select="number($args/value/malval/lvalue/malval[1]/@value) div number($args/value/malval/lvalue/malval[2]/@value)"/>
        <xsl:sequence select="fn:makeMALType($result, 'number')"/>
      </xsl:when>
      <xsl:otherwise>
        <error>Invalid function <xsl:sequence select="$func"/> </error>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="EVAL">
    <xsl:param name="env"/>
    <xsl:choose>
      <xsl:when test="value/malval/@kind = 'list'">
        <xsl:choose>
          <xsl:when test="count(value/malval/lvalue/malval) = 0">
            <xsl:sequence select="."/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:variable name="new_list">
              <xsl:call-template name="eval_ast">
                <xsl:with-param name="env" select="$env"/>
              </xsl:call-template>
            </xsl:variable>
            <xsl:variable name="func">
              <xsl:for-each select="$new_list">
                <xsl:sequence select="value/malval/lvalue[1]/malval/text()"/>
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
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="eval_ast">
          <xsl:with-param name="env" select="$env"/>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
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
  <xsl:function name="fn:env_lookup">
    <xsl:param name="env"/>
    <xsl:param name="name"/>
    <xsl:variable name="value">
      <xsl:sequence select="$env(string($name))"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="$value != ''">
        <malval kind="nfunction" name="{$name}">
          <xsl:sequence select="$value"/>
        </malval>
      </xsl:when>
      <xsl:otherwise>
        <malval kind="nil"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <xsl:function name="fn:makeMALType">
    <xsl:param name="value"/>
    <xsl:param name="kind"/>
    <value>
      <malval kind="{$kind}" value="{$value}"/>
    </value>
  </xsl:function>
</xsl:stylesheet>
