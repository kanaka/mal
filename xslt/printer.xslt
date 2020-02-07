<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="3.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <!-- expects input as a single <value><malval .../></value> -->
    <!-- output of form <value>literal string</value> -->
    <xsl:template name="malprinter-pr_str">
        <xsl:variable name="value">
            <xsl:copy-of select="value/malval" />
        </xsl:variable> 
        <xsl:for-each select="$value">
            <xsl:choose>
            <xsl:when test="malval/@kind = 'string'">
                <value>
                    <xsl:value-of select="concat('&quot;', malval/@value, '&quot;')" />
                </value>
            </xsl:when>
            <xsl:when test="malval/@kind = 'symbol'">
                <value>
                    <xsl:value-of select="malval/@value" />
                </value>
            </xsl:when>
            <xsl:when test="malval/@kind = 'number'">
                <value>
                    <xsl:value-of select="malval/@value" />
                </value>
            </xsl:when>
            <xsl:when test="malval/@kind = 'list'">
                <xsl:variable name="val">
                    <xsl:for-each select="malval/lvalue/malval">
                        <xsl:variable name="ctx">
                            <value><xsl:copy-of select="." /></value>
                        </xsl:variable>
                        <xsl:for-each select="$ctx">
                            <xsl:call-template name="malprinter-pr_str"></xsl:call-template>
                        </xsl:for-each>
                    </xsl:for-each>
                </xsl:variable>
                <xsl:for-each select="$val">
                    <value>
                        <xsl:value-of select="concat('(', string-join(/value, ' '), ')')" />
                    </value>
                </xsl:for-each>
            </xsl:when>
            <xsl:otherwise>
                <value>Unknown <xsl:copy-of select="." /></value>
            </xsl:otherwise>
            </xsl:choose>
        </xsl:for-each>
    </xsl:template>

    <xsl:template name="malprinter-pr_list">
    </xsl:template>
</xsl:stylesheet>