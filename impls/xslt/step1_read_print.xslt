<?xml version="1.0" encoding="UTF-8"?>
<!-- A way to keep a process waiting until we signal it -->
<!-- In order to have a process pool that executes our queries faster -->
<xsl:stylesheet version="3.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:import href="step1_read_print.inc.xslt"></xsl:import>
<xsl:param name="process_id" required="yes"/>
<xsl:template match="/">
  <xsl:variable name="uri" select="concat('process-input-', $process_id, '.xml')"></xsl:variable>
  <xsl:variable name="_lock" select="unparsed-text(concat('process-lock-', $process_id))"></xsl:variable>
  <!-- use _lock to trick the runtime -->
  <xsl:variable name="ctx" select="doc(concat($uri, substring($_lock, 0, 0)))"/>
  <xsl:for-each select="$ctx/mal">
    <xsl:call-template name="rep"/>
  </xsl:for-each>
</xsl:template>
</xsl:stylesheet>