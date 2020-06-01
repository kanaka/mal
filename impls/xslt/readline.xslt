<?xml version="1.0" encoding="UTF-8"?>
<!-- Readline: interface for talking to the harness about readline -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:readline="READLINE" version="3.0">
  <xsl:function name="readline:readline">
    <xsl:param name="prompt"/>
    <xsl:message>
      <request kind="readline" value="{$prompt}"/>
    </xsl:message>
    <xsl:variable name="value" select="unparsed-text('xsl_input-string')"/>
    <xsl:sequence select="$value"/>
  </xsl:function>
</xsl:stylesheet>
