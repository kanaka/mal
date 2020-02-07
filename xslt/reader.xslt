<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="3.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <!-- Expects a "tokens" in current scope -->
    <xsl:template name="malreader-peek">
        <!-- <xsl:message>PEEK <xsl:copy-of select=".">
              
            </xsl:copy-of>
        
        ;</xsl:message> -->
      <xsl:variable name="context">
        <tokens>
            <xsl:copy-of select="tokens/*" />
        </tokens>
        <value>
            <xsl:for-each select="tokens/token[1]">
                <xsl:copy-of select="."></xsl:copy-of>
            </xsl:for-each>
        </value>
        <xsl:copy-of select="lvalue"></xsl:copy-of>
        <xsl:copy-of select="error"></xsl:copy-of>
      </xsl:variable>
      <xsl:for-each select="$context"><xsl:copy-of select="." /></xsl:for-each>
    </xsl:template>


    <xsl:template name="malreader-next">
        <!-- <xsl:message>NEXT <xsl:copy-of select=".">
              
            </xsl:copy-of>
        
        ;
        </xsl:message> -->
      <xsl:variable name="context">
        <tokens>
            <xsl:for-each select="tokens/token[position() != 1]">
                <xsl:copy-of select="." />
            </xsl:for-each>
        </tokens>
        <value>
            <xsl:for-each select="tokens/token[1]">
                <xsl:copy-of select="."></xsl:copy-of>
            </xsl:for-each>
        </value>
        <xsl:copy-of select="lvalue"></xsl:copy-of>
        <xsl:copy-of select="error"></xsl:copy-of>
      </xsl:variable>
      <xsl:for-each select="$context"><xsl:copy-of select="./*" /></xsl:for-each>
    </xsl:template>

    <xsl:template name="malreader-read_str">
        <xsl:variable name="context">
            <input value="{str}"/>
            <tokens>
                <xsl:call-template name="malreader-tokenize"></xsl:call-template>
            </tokens>
        </xsl:variable>
        <xsl:for-each select="$context">
            <xsl:call-template name="malreader-read_form"></xsl:call-template>
        </xsl:for-each>
        <!-- <xsl:copy-of select="$context" /> -->
    </xsl:template>

    <xsl:template name="malreader-tokenize">
        <xsl:analyze-string select="str" regex="[\s,]*(~@|[\[\]{{}}()'`~^@]|&quot;(?:\\.|[^\\&quot;])*&quot;?|;.*|[^\s\[\]{{}}('&quot;`,;)]+)" flags=";j">
            <xsl:matching-substring>
                <xsl:variable name="match">
                  <xsl:copy-of select="regex-group(1)" />
                </xsl:variable>
                <xsl:if test="string-length($match) > 0">
                    <xsl:choose>
                        <xsl:when test="starts-with($match, '~@')">
                            <token type="special" text="~@" />
                        </xsl:when>
                        <xsl:when test="matches($match, '[\[\]\{\}()''`~^@]')">
                            <token type="special" text="{$match}"></token>
                        </xsl:when>
                        <xsl:when test="starts-with($match, '&quot;')">
                            <xsl:choose>
                                <xsl:when test="ends-with($match, '&quot;')">
                                    <token type="string" text="{replace($match, '&quot;(.*)&quot;', '$1')}"> </token>
                                </xsl:when>
                                <xsl:otherwise>
                                    <token type="error" text="Unbalanced quotes"></token>
                                </xsl:otherwise>
                            </xsl:choose>
                        </xsl:when>
                        <xsl:when test="starts-with($match, ':')">
                            <token type="keyword" text="{replace($match, ':(.*)', '$1')}"/>
                        </xsl:when>
                        <xsl:when test="starts-with($match, ';')">
                          <!-- ignore comments -->
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:choose>
                                <xsl:when test="$match = 'false'">
                                    <token type="false"></token>
                                </xsl:when>
                                <xsl:when test="$match = 'true'">
                                    <token type="true"></token>
                                </xsl:when>
                                <xsl:when test="$match = 'nil'">
                                    <token type="nil"></token>
                                </xsl:when>
                                <xsl:when test="matches($match, '^\d+$')">
                                    <token type="number" text="{$match}"></token>
                                </xsl:when>
                                <xsl:otherwise>
                                    <token type="symbol" text="{$match}"></token>
                                </xsl:otherwise>
                            </xsl:choose>
                        </xsl:otherwise>
                    </xsl:choose>
                </xsl:if>
            </xsl:matching-substring>
        </xsl:analyze-string>
    </xsl:template>

    <xsl:template name="malreader-read_form">
        <xsl:variable name="peek">
            <xsl:call-template name="malreader-peek"></xsl:call-template>
        </xsl:variable>
        <xsl:for-each select="$peek">
            <xsl:choose>
                <xsl:when test="value/token/@text = '('">
                    <xsl:variable name="next">
                        <xsl:call-template name="malreader-next"></xsl:call-template>
                    </xsl:variable>
                    <xsl:for-each select="$next">
                        <xsl:variable name="listkind">
                            <xsl:value-of select="value/token/@text" /> <!-- listkind [/(/{ -->
                        </xsl:variable>
                        <xsl:call-template name="malreader-read_list" />
                    </xsl:for-each>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:call-template name="malreader-read_atom"></xsl:call-template>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:for-each>
    </xsl:template>

    <xsl:template name="malreader-read_list">
      <xsl:variable name="value">
        <xsl:call-template name="malreader-read_list_helper"></xsl:call-template>
      </xsl:variable>
      <xsl:for-each select="$value">
        <xsl:copy-of select="tokens" />
        <xsl:copy-of select="error" />
        <value>
            <malval kind="list">
                <xsl:copy-of select="lvalue[1]"/>
            </malval>
        </value>
      </xsl:for-each>
    </xsl:template>

    <xsl:template name="malreader-read_list_helper">
        <xsl:choose>
          <xsl:when test="count(tokens/*) > 0">
            <xsl:variable name="peek">
                <xsl:call-template name="malreader-peek"></xsl:call-template>
            </xsl:variable>
            <xsl:for-each select="$peek">
                <xsl:choose>
                    <xsl:when test="value/token/@text = ')' and value/token/@type = 'special'">
                        <!-- ok -->
                        <xsl:variable name="next">
                            <xsl:call-template name="malreader-next"></xsl:call-template>
                        </xsl:variable>
                        <xsl:copy-of select="lvalue" />
                        <xsl:copy-of select="$next/tokens" />
                        <xsl:copy-of select="error" />
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:variable name="form">
                            <xsl:call-template name="malreader-read_form"></xsl:call-template>
                        </xsl:variable>
                        <xsl:variable name="context">
                            <xsl:for-each select="$form">
                            <!-- <xsl:message>READ_FORM <xsl:copy-of select=".">
                                  
                                </xsl:copy-of>
                            
                            ;</xsl:message> -->
                                <xsl:copy-of select="tokens" />
                                <lvalue>
                                    <xsl:for-each select="lvalue/malval"><xsl:copy-of select="." /></xsl:for-each>
                                    <xsl:for-each select="value/malval"><xsl:copy-of select="."/></xsl:for-each>
                                </lvalue>
                            </xsl:for-each>
                        </xsl:variable>
                        <xsl:for-each select="$context">
                            <xsl:call-template name="malreader-read_list_helper"/>
                        </xsl:for-each>
                    </xsl:otherwise>
                </xsl:choose>
                <xsl:copy-of select="lvalue"/>
            </xsl:for-each>
          </xsl:when>
          <xsl:otherwise>
              <error><malval kind="error">Unbalanced input</malval></error>
          </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="malreader-read_atom">
        <xsl:variable name="next">
          <xsl:call-template name="malreader-next"></xsl:call-template>
        </xsl:variable>
        <xsl:for-each select="$next">
            <xsl:copy-of select="tokens"/>
            <xsl:copy-of select="lvalue"/>
            <xsl:copy-of select="error"/>
            <xsl:choose>
                <xsl:when test="value/token/@type = 'number'">
                    <value>
                        <malval kind="number" value="{value/token/@text}" />
                    </value>
                </xsl:when>
                <xsl:when test="value/token/@type = 'symbol'">
                    <value>
                        <malval kind="symbol" value="{value/token/@text}" />
                    </value>
                </xsl:when>
                <xsl:when test="value/token/@type = 'string'">
                    <value>
                        <malval kind="string" value="{value/token/@text}" />
                    </value>
                </xsl:when>
                <xsl:when test="value/token/@type = 'keyword'">
                    <value>
                        <malval kind="keyword" value="{value/token/@text}" />
                    </value>
                </xsl:when>
                <xsl:when test="value/token/@type = 'true'">
                    <value>
                        <malval kind="true"/>
                    </value>
                </xsl:when>
                <xsl:when test="value/token/@type = 'false'">
                    <value>
                        <malval kind="false"/>
                    </value>
                </xsl:when>
                <xsl:when test="value/token/@type = 'nil'">
                    <value>
                        <malval kind="nil"/>
                    </value>
                </xsl:when>
                <xsl:otherwise>
                    <value>
                        <malval kind="error" value="{value/token}" />
                    </value>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:for-each>
    </xsl:template>
</xsl:stylesheet>
