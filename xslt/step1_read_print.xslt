<?xml version="1.0" encoding="UTF-8"?>
<!-- Step 1: Read-Print -->
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
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:import href="reader.xslt" />
    <xsl:import href="printer.xslt" />
    <xsl:output method='xml' encoding='utf-8' indent='yes'/>
    <xsl:template match="mal" name="rep">
      <mal>
        <stdin></stdin> <!-- clear stdin -->
        <xsl:copy-of select="state" /> <!-- preserve state -->
        <stdout><xsl:call-template name="PRINT" /></stdout>
      </mal>
    </xsl:template>

    <xsl:template name="PRINT">
      <xsl:variable name="context">
        <xsl:variable name="ctx">
          <xsl:call-template name="EVAL" />
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

    <xsl:template name="EVAL">
      <xsl:variable name="context">
        <xsl:call-template name="READ" />
      </xsl:variable>
      <xsl:for-each select="$context"><xsl:copy-of select="." /></xsl:for-each>
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
</xsl:stylesheet>
