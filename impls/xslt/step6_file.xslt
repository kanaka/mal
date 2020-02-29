<?xml version="1.0" encoding="UTF-8"?>
<!-- A way to keep a process waiting until we signal it -->
<!-- In order to have a process pool that executes our queries faster -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:readline="READLINE" version="3.0" xmlns:err="http://www.w3.org/2005/xqt-errors" exclude-result-prefixes="readline err">
  <xsl:import href="step6_file.inc.xslt"/>
  <xsl:import href="readline.xslt"/>
  <xsl:template match="/">
    <xsl:variable name="ctx" select="."/>
    <xsl:variable name="in_repl" select="empty($ctx/mal/no_repl)"></xsl:variable>
    <xsl:iterate select="1 to (if ($in_repl) then 100000000 else 1)">
      <xsl:param name="val" select="$ctx"/>
      <xsl:on-completion>
        <xsl:sequence select="$val/*"/>
      </xsl:on-completion>
      <xsl:variable name="line" select="readline:readline('user&gt; ')"/>
      <xsl:variable name="ctx">
        <mal>
          <stdin>
            <xsl:value-of select="$line"/>
          </stdin>
          <xsl:sequence select="$val/mal/argv"/>
          <xsl:sequence select="$val/mal/state"/>
        </mal>
      </xsl:variable>
      <xsl:variable name="val">
        <xsl:try>
          <xsl:for-each select="$ctx/mal">
            <xsl:call-template name="rep">
              <xsl:with-param name="display" select="$in_repl"/>
            </xsl:call-template>
          </xsl:for-each>
          <xsl:catch errors="*">
            <xsl:message>
              <request kind="display" value="Error: {$err:description}"/>
            </xsl:message>
            <xsl:sequence select="$ctx"/>
          </xsl:catch>
        </xsl:try>
      </xsl:variable>
      <xsl:next-iteration>
        <xsl:with-param name="val" select="$val"/>
      </xsl:next-iteration>
    </xsl:iterate>
  </xsl:template>
</xsl:stylesheet>
