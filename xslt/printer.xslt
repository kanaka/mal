<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="3.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:fn="http://www.w3.org/2005/02/xpath-functions">
    <!-- expects input as a single <value><malval .../></value> -->
    <!-- output of form <value>literal string</value> -->
    <xsl:template name="malprinter-pr_str">
        <xsl:param name="readably" as="xs:boolean" />

        <xsl:variable name="value">
            <xsl:copy-of select="value/malval" />
        </xsl:variable> 
        <xsl:for-each select="$value">
            <xsl:choose>
            <xsl:when test="malval/@kind = 'true'">
                <value>true</value>
            </xsl:when>
            <xsl:when test="malval/@kind = 'false'">
                <value>false</value>
            </xsl:when>
            <xsl:when test="malval/@kind = 'nil'">
                <value>nil</value>
            </xsl:when>
            <xsl:when test="malval/@kind = 'string'">
                <value>
                    <xsl:value-of select="concat('&quot;', fn:desc_string(malval/@value, $readably), '&quot;')" />
                </value>
            </xsl:when>
            <xsl:when test="malval/@kind = 'keyword'">
                <value>
                    <xsl:value-of select="concat(':', malval/@value)" />
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
                            <xsl:call-template name="malprinter-pr_str"><xsl:with-param name="readably" select="$readably"/></xsl:call-template>
                        </xsl:for-each>
                    </xsl:for-each>
                </xsl:variable>
                <xsl:for-each select="$val">
                    <value>
                        <xsl:value-of select="concat('(', string-join(/value, ' '), ')')" />
                    </value>
                </xsl:for-each>
            </xsl:when>
            <xsl:when test="malval/@kind = 'vector'">
                <xsl:variable name="val">
                    <xsl:for-each select="malval/lvalue/malval">
                        <xsl:variable name="ctx">
                            <value><xsl:copy-of select="." /></value>
                        </xsl:variable>
                        <xsl:for-each select="$ctx">
                            <xsl:call-template name="malprinter-pr_str"><xsl:with-param name="readably" select="$readably"/></xsl:call-template>
                        </xsl:for-each>
                    </xsl:for-each>
                </xsl:variable>
                <xsl:for-each select="$val">
                    <value>
                        <xsl:value-of select="concat('[', string-join(/value, ' '), ']')" />
                    </value>
                </xsl:for-each>
            </xsl:when>
            <xsl:when test="malval/@kind = 'hash'">
                <xsl:variable name="val">
                    <xsl:for-each select="malval/lvalue/malval">
                        <xsl:variable name="ctx">
                            <value><xsl:copy-of select="." /></value>
                        </xsl:variable>
                        <xsl:for-each select="$ctx">
                            <xsl:call-template name="malprinter-pr_str"><xsl:with-param name="readably" select="$readably"/></xsl:call-template>
                        </xsl:for-each>
                    </xsl:for-each>
                </xsl:variable>
                <xsl:for-each select="$val">
                    <value>
                        <xsl:value-of select="concat('{', string-join(/value, ' '), '}')" />
                    </value>
                </xsl:for-each>
            </xsl:when>
            <xsl:otherwise>
                <value>Unknown <xsl:copy-of select="." /></value>
            </xsl:otherwise>
            </xsl:choose>
        </xsl:for-each>
    </xsl:template>

    <xsl:function name="fn:desc_string" as="xs:string">
        <xsl:param name="str" as="xs:string" />
        <xsl:param name="readable" as="xs:boolean" />
        <xsl:choose>
          <xsl:when test="not($readable)">
            <xsl:variable name="sx">
                <xsl:analyze-string select="$str" regex="\\(n|&quot;|\\)">
                    <xsl:matching-substring>
                        <x>
                            <xsl:choose>
                              <xsl:when test="regex-group(1) = 'n'">
                                <xsl:value-of select="'&#10;'" />
                              </xsl:when>
                              <xsl:otherwise>
                                <xsl:value-of select="regex-group(1)" />
                              </xsl:otherwise>
                            </xsl:choose>
                        </x>
                    </xsl:matching-substring>
                    <xsl:non-matching-substring><x><xsl:value-of select="." /></x></xsl:non-matching-substring>
                </xsl:analyze-string>
            </xsl:variable>
            <xsl:value-of select="string-join($sx/x, '')" />
          </xsl:when>
          <xsl:otherwise>
              <xsl:value-of select="$str" />
          </xsl:otherwise>
        </xsl:choose>
    </xsl:function>
</xsl:stylesheet>