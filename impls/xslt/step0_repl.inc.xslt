<?xml version="1.0" encoding="UTF-8"?>
<!-- Step 0: REPL -->
<!-- input document must be in the following format -->
<!--
<mal>
    <stdin>...stdin text...</stdin>
    <stdout> ... ignored, omitted ... </stdout>
    <state> ignored, preserved </state>
</mal>
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="xml" encoding="utf-8" indent="yes"/>
  <xsl:template match="mal" name="rep">
    <xsl:param name="display" select="false()" />
    <mal>
      <stdin/>
      <!-- clear stdin -->
      <xsl:copy-of select="state"/>
      <!-- preserve state -->
      <xsl:variable name="_read">
        <xsl:call-template name="READ"/>
      </xsl:variable>
      <xsl:variable name="_eval">
        <xsl:for-each select="$_read">
          <xsl:call-template name="EVAL"/>
        </xsl:for-each>
      </xsl:variable>
      <xsl:variable name="_print">
        <xsl:for-each select="$_eval">
          <xsl:call-template name="PRINT"/>
        </xsl:for-each>
      </xsl:variable>
      <xsl:message>
        <request kind="display" value="{$_print}"/>
      </xsl:message>
    </mal>
  </xsl:template>
  <xsl:template name="PRINT">
    <xsl:sequence select="."/>
  </xsl:template>
  <xsl:template name="EVAL">
    <xsl:sequence select="."/>
  </xsl:template>
  <xsl:template name="READ">
    <xsl:copy-of select="stdin/text()"/>
  </xsl:template>
</xsl:stylesheet>
