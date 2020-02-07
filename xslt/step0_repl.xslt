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
<xsl:stylesheet
    version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:output method='xml' encoding='utf-8' indent='yes'/>
    <xsl:template match="mal" name="rep">
      <mal>
        <stdin></stdin> <!-- clear stdin -->
        <xsl:copy-of select="state" /> <!-- preserve state -->
        <stdout><xsl:call-template name="PRINT" /></stdout> <!-- copy stdin to stdout -->
      </mal>
    </xsl:template>
    <xsl:template name="PRINT">
        <xsl:call-template name="EVAL" />
    </xsl:template>
    <xsl:template name="EVAL">
      <xsl:call-template name="READ" />
    </xsl:template>
    <xsl:template name="READ">
        <xsl:copy-of select="stdin/text()" />
    </xsl:template>
</xsl:stylesheet>
