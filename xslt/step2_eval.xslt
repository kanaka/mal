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
<xsl:stylesheet
    version="3.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:fn="http://www.w3.org/2005/02/xpath-functions" xmlns:xs="http://www.w3.org/2001/XMLSchema"  xmlns:map="http://www.w3.org/2005/xpath-functions/map">
    <xsl:import href="reader.xslt" />
    <xsl:import href="printer.xslt" />
    <xsl:output method='xml' encoding='utf-8' indent='yes'/>
    <xsl:template match="mal" name="rep">
      <mal>
        <xsl:variable name="env" as="map(*)">
          <xsl:if test="not(state)"> <!-- we never update this -->
            <xsl:map>
              <xsl:map-entry key="'+'" select="'lvalue/malval[1]/@value + lvalue/malval[2]/@value'" />
              <xsl:map-entry key="'-'" select="'lvalue/malval[1]/@value - lvalue/malval[2]/@value'" />
              <xsl:map-entry key="'*'" select="'lvalue/malval[1]/@value * lvalue/malval[2]/@value'" />
              <xsl:map-entry key="'/'" select="'lvalue/malval[1]/@value / lvalue/malval[2]/@value'" />
            </xsl:map>
          </xsl:if>
        </xsl:variable>
        <xsl:sequence select="stdin"/>
        <stdout><xsl:call-template name="PRINT"><xsl:with-param name="env" select="$env"/></xsl:call-template></stdout> 
      </mal>
    </xsl:template>

    <xsl:template name="PRINT">
      <xsl:param name="env" />
      <xsl:variable name="context">
        <xsl:variable name="ctx">
          <xsl:call-template name="EVAL"><xsl:with-param name="env" select="$env"/></xsl:call-template>
        </xsl:variable>
        <xsl:for-each select="$ctx">
          <xsl:variable name="str">
            <xsl:call-template name="malprinter-pr_str"><xsl:with-param name="readably" select="true()"/></xsl:call-template>
          </xsl:variable>
          <xsl:value-of select="$str" />
        </xsl:for-each>
      </xsl:variable>
      <xsl:for-each select="$context"><xsl:copy-of select="." /></xsl:for-each>
    </xsl:template>

    <xsl:template name="eval_ast">
      <xsl:param name="env" />
      <xsl:choose>
        <xsl:when test="value/malval/@kind = 'symbol'">
          <value><xsl:sequence select="fn:env_lookup($env, value/malval/@value)" /></value>
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
                      <xsl:call-template name="eval_ast"><xsl:with-param name="env" select="$env"/></xsl:call-template>
                    </xsl:for-each>
                  </xsl:variable>
                  <xsl:sequence select="$xctx/value/malval"/>
                </xsl:for-each>
              </lvalue>
            </malval>
          </value>
        </xsl:when>
        <xsl:otherwise>
          <xsl:sequence select="." />
        </xsl:otherwise>
      </xsl:choose>
    </xsl:template>

    <xsl:template name="EVAL">
      <xsl:param name="env" />
      <xsl:variable name="context">
        <xsl:call-template name="READ" />
      </xsl:variable>
      <xsl:for-each select="$context">
        <xsl:choose>
          <xsl:when test="value/malval/@kind = 'list'">
            <xsl:choose>
              <xsl:when test="count(value/malval/lvalue/malval) = 0">
                <xsl:sequence select="."/>
              </xsl:when>
              <xsl:otherwise>
                <!-- <xsl:variable name="new_list"> -->
                  <xsl:call-template name="eval_ast"><xsl:with-param name="env" select="$env"/></xsl:call-template>
                <!-- </xsl:variable>
                <xsl:sequence select="$new_list"/> -->
              </xsl:otherwise>
            </xsl:choose>
          </xsl:when>
          <xsl:otherwise>
            <xsl:sequence select="."/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:for-each>
    </xsl:template>

    <xsl:template name="READ">
      <xsl:variable name="context">
          <str>
              <xsl:copy-of select="stdin/text()" />
          </str>
      </xsl:variable>
      <xsl:variable name="form">
        <xsl:for-each select="$context">
            <xsl:call-template name="malreader-read_str"></xsl:call-template>
        </xsl:for-each>
      </xsl:variable>
      <xsl:for-each select="$form">
        <xsl:if test="error">
          <xsl:message terminate="yes">
            <xsl:value-of select="error" />
          </xsl:message>
        </xsl:if>
        <xsl:copy-of select="." />
      </xsl:for-each>
    </xsl:template>

    <xsl:function name="fn:env_lookup">
      <xsl:param name="env" />
      <xsl:param name="name" />
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
</xsl:stylesheet>
