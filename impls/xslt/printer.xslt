<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:fn="http://www.w3.org/2005/02/xpath-functions" version="3.0" exclude-result-prefixes="xsl xs fn">
  <!-- expects input as a single <value><malval .../></value> -->
  <!-- output of form <value>literal string</value> -->
  <xsl:template name="malprinter-pr_str">
    <xsl:param name="readably" as="xs:boolean"/>
    <xsl:variable name="value">
      <xsl:sequence select="value/malval"/>
      <xsl:sequence select="atoms"/>
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
            <xsl:value-of select="concat(
                                        if ($readably) then
                                            '&quot;'
                                        else
                                            '',

                                        fn:desc_string(malval/@value, $readably),

                                        if ($readably) then
                                            '&quot;'
                                        else
                                            '')"/>
          </value>
        </xsl:when>
        <xsl:when test="malval/@kind = 'keyword'">
          <value>
            <xsl:value-of select="concat(':', malval/@value)"/>
          </value>
        </xsl:when>
        <xsl:when test="malval/@kind = 'symbol'">
          <value>
            <xsl:value-of select="malval/@value"/>
          </value>
        </xsl:when>
        <xsl:when test="malval/@kind = 'number'">
          <value>
            <xsl:value-of select="malval/@value"/>
          </value>
        </xsl:when>
        <xsl:when test="malval/@kind = 'list'">
          <xsl:variable name="val">
            <xsl:for-each select="malval/lvalue/malval">
              <xsl:variable name="ctx">
                <value>
                  <xsl:copy-of select="."/>
                </value>
              </xsl:variable>
              <xsl:for-each select="$ctx">
                <xsl:call-template name="malprinter-pr_str">
                  <xsl:with-param name="readably" select="$readably"/>
                </xsl:call-template>
              </xsl:for-each>
            </xsl:for-each>
          </xsl:variable>
          <xsl:for-each select="$val">
            <value>
              <xsl:value-of select="concat('(', string-join(/value, ' '), ')')"/>
            </value>
          </xsl:for-each>
        </xsl:when>
        <xsl:when test="malval/@kind = 'vector'">
          <xsl:variable name="val">
            <xsl:for-each select="malval/lvalue/malval">
              <xsl:variable name="ctx">
                <value>
                  <xsl:copy-of select="."/>
                </value>
              </xsl:variable>
              <xsl:for-each select="$ctx">
                <xsl:call-template name="malprinter-pr_str">
                  <xsl:with-param name="readably" select="$readably"/>
                </xsl:call-template>
              </xsl:for-each>
            </xsl:for-each>
          </xsl:variable>
          <xsl:for-each select="$val">
            <value>
              <xsl:value-of select="concat('[', string-join(/value, ' '), ']')"/>
            </value>
          </xsl:for-each>
        </xsl:when>
        <xsl:when test="malval/@kind = 'hash'">
          <xsl:variable name="val">
            <xsl:for-each select="malval/lvalue/malval">
              <xsl:variable name="ctx">
                <value>
                  <xsl:copy-of select="."/>
                </value>
              </xsl:variable>
              <xsl:for-each select="$ctx">
                <xsl:call-template name="malprinter-pr_str">
                  <xsl:with-param name="readably" select="$readably"/>
                </xsl:call-template>
              </xsl:for-each>
            </xsl:for-each>
          </xsl:variable>
          <xsl:for-each select="$val">
            <value>
              <xsl:value-of select="concat('{', string-join(/value, ' '), '}')"/>
            </value>
          </xsl:for-each>
        </xsl:when>
        <xsl:when test="malval/@kind = 'function'">
          <value>
            <xsl:variable name="gt">?</xsl:variable>
            <xsl:variable name="lt">?</xsl:variable>
            <xsl:value-of select="concat('#', $lt, 'fn ', malval/@name, $gt)"/>
          </value>
        </xsl:when>
        <xsl:when test="malval/@kind = 'userfunction'">
          <value>
            <xsl:variable name="gt">?</xsl:variable>
            <xsl:variable name="lt">?</xsl:variable>
            <xsl:value-of select="concat('#', $lt, 'function', $gt)"/>
          </value>
        </xsl:when>
        <xsl:when test="malval/@kind = 'atom'">
          <xsl:variable name="val" select="malval"/>
          <xsl:variable name="inner">
            <xsl:variable name="ctx">
              <value>
                <xsl:sequence select="atoms/atom[@identity = $val/@value]/malval"/>
              </value>
            </xsl:variable>
            <xsl:for-each select="$ctx">
              <xsl:call-template name="malprinter-pr_str">
                <xsl:with-param name="readably" select="$readably"/>
              </xsl:call-template>
            </xsl:for-each>
          </xsl:variable>
          <value>
            <xsl:value-of select="concat(
                                      '(atom ',
                                      (let $v := $inner/value
                                            return if ($v = 'Unknown') then
                                                     concat('id=', string($val/@value), ' existing=', string-join(atoms/atom/@identity, ','))
                                                   else
                                                      $v),
                                      ')')"/>
          </value>
        </xsl:when>
        <xsl:otherwise>
          <value>Unknown</value>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>
  <xsl:function name="fn:desc_string" as="xs:string">
    <xsl:param name="str" as="xs:string"/>
    <xsl:param name="readable" as="xs:boolean"/>
    <xsl:choose>
      <xsl:when test="($readable)">
        <xsl:variable name="sx">
          <xsl:analyze-string select="$str" regex="(\\|&quot;|&#10;)">
            <xsl:matching-substring>
              <x>
                <xsl:choose>
                  <xsl:when test="regex-group(1) = '\'">
                    <xsl:value-of select="'\\'"/>
                  </xsl:when>
                  <xsl:when test="regex-group(1) = '&quot;'">
                    <xsl:value-of select="'\&quot;'"/>
                  </xsl:when>
                  <xsl:otherwise>
                    <xsl:value-of select="'\n'"/>
                  </xsl:otherwise>
                </xsl:choose>
              </x>
            </xsl:matching-substring>
            <xsl:non-matching-substring>
              <x>
                <xsl:value-of select="."/>
              </x>
            </xsl:non-matching-substring>
          </xsl:analyze-string>
        </xsl:variable>
        <xsl:value-of select="string-join($sx/x, '')"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$str"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
</xsl:stylesheet>
