<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:fn="http://www.w3.org/2005/02/xpath-functions" version="3.0" exclude-result-prefixes="xsl xs fn">
  <!-- Expects a "tokens" in current scope -->
  <xsl:template name="malreader-peek">
    <!-- <xsl:message>PEEK <xsl:sequence select=".">
              
            </xsl:sequence>
        
        ;</xsl:message> -->
    <xsl:variable name="context">
      <tokens>
        <xsl:sequence select="tokens/*"/>
      </tokens>
      <value>
        <xsl:for-each select="tokens/token[1]">
          <xsl:sequence select="."/>
        </xsl:for-each>
      </value>
      <xsl:sequence select="lvalue"/>
      <xsl:sequence select="error"/>
    </xsl:variable>
    <xsl:for-each select="$context">
      <xsl:sequence select="."/>
    </xsl:for-each>
  </xsl:template>
  <xsl:template name="malreader-next">
    <!-- <xsl:message>NEXT <xsl:sequence select=".">
              
            </xsl:sequence>
        
        ;
        </xsl:message> -->
    <xsl:variable name="context">
      <tokens>
        <xsl:for-each select="tokens/token[position() != 1]">
          <xsl:sequence select="."/>
        </xsl:for-each>
      </tokens>
      <value>
        <xsl:for-each select="tokens/token[1]">
          <xsl:sequence select="."/>
        </xsl:for-each>
      </value>
      <xsl:sequence select="lvalue"/>
      <xsl:sequence select="error"/>
    </xsl:variable>
    <xsl:for-each select="$context">
      <xsl:sequence select="./*"/>
    </xsl:for-each>
  </xsl:template>
  <xsl:template name="malreader-read_str">
    <xsl:variable name="context">
      <input value="{str}"/>
      <tokens>
        <xsl:call-template name="malreader-tokenize"/>
      </tokens>
    </xsl:variable>
    <xsl:for-each select="$context">
      <xsl:call-template name="malreader-read_form"/>
    </xsl:for-each>
    <!-- <xsl:sequence select="$context" /> -->
  </xsl:template>
  <xsl:template name="malreader-tokenize">
    <xsl:analyze-string select="str" regex="[\s,]*(~@|[\[\]{{}}()'`~^@]|&quot;(?:\\.|[^\\&quot;])*&quot;?|;.*|[^\s\[\]{{}}('&quot;`,;)]+)" flags=";j">
      <xsl:matching-substring>
        <xsl:variable name="match">
          <xsl:sequence select="regex-group(1)"/>
        </xsl:variable>
        <xsl:if test="string-length($match) &gt; 0">
          <xsl:choose>
            <xsl:when test="starts-with($match, '&quot;')">
              <xsl:choose>
                <xsl:when test="fn:check_string($match)">
                  <token type="error" text="EOF while reading string or invalid escape in string"/>
                </xsl:when>
                <xsl:when test="ends-with($match, '&quot;')">
                  <token type="string" text="{fn:process-string(replace($match, '&quot;((.|\s)*)&quot;', '$1'))}"> </token>
                </xsl:when>
                <xsl:otherwise>
                  <token type="error" text="EOF while reading string"/>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:when>
            <xsl:when test="starts-with($match, ':')">
              <token type="keyword" text="{replace($match, ':(.*)', '$1')}"/>
            </xsl:when>
            <xsl:when test="starts-with($match, ';')">
              <!-- ignore comments -->
            </xsl:when>
            <xsl:when test="starts-with($match, '~@')">
              <token type="special" text="~@"/>
            </xsl:when>
            <xsl:when test="matches($match, '[\[\]\{\}()''`~^@]')">
              <token type="special" text="{$match}"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:choose>
                <xsl:when test="$match = 'false'">
                  <token type="false"/>
                </xsl:when>
                <xsl:when test="$match = 'true'">
                  <token type="true"/>
                </xsl:when>
                <xsl:when test="$match = 'nil'">
                  <token type="nil"/>
                </xsl:when>
                <xsl:when test="matches($match, '^-?\d+$')">
                  <token type="number" text="{$match}"/>
                </xsl:when>
                <xsl:otherwise>
                  <token type="symbol" text="{$match}"/>
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
      <xsl:call-template name="malreader-peek"/>
    </xsl:variable>
    <xsl:variable name="next">
      <xsl:call-template name="malreader-next"/>
    </xsl:variable>
    <xsl:for-each select="$peek">
      <xsl:choose>
        <xsl:when test="value/token/@type = 'error'">
          <error>
            <malval kind="error">
              <xsl:value-of select="value/token/@text"/>
            </malval>
          </error>
        </xsl:when>
        <xsl:when test="contains('([{', value/token/@text) and value/token/@type = 'special'">
          <xsl:variable name="next">
            <xsl:call-template name="malreader-next"/>
          </xsl:variable>
          <xsl:for-each select="$next">
            <xsl:variable name="listkind">
              <xsl:value-of select="value/token/@text"/>
              <!-- listkind [/(/{ -->
            </xsl:variable>
            <xsl:call-template name="malreader-read_list">
              <xsl:with-param name="listkind" select="$listkind"/>
            </xsl:call-template>
          </xsl:for-each>
        </xsl:when>
        <xsl:when test="value/token/@text = &quot;'&quot; and value/token/@type = 'special'">
          <xsl:for-each select="$next">
            <xsl:variable name="inner">
              <xsl:variable name="ctx">
                <xsl:sequence select="tokens"/>
              </xsl:variable>
              <xsl:for-each select="$ctx">
                <xsl:call-template name="malreader-read_form"/>
              </xsl:for-each>
            </xsl:variable>
            <xsl:for-each select="$inner">
              <value>
                <malval kind="list">
                  <lvalue>
                    <malval kind="symbol" value="quote"/>
                    <xsl:sequence select="/value/malval"/>
                  </lvalue>
                </malval>
              </value>
              <xsl:sequence select="tokens"/>
              <xsl:sequence select="error"/>
            </xsl:for-each>
            <xsl:sequence select="lvalue"/>
            <!-- preserve previous list (if any) -->
          </xsl:for-each>
        </xsl:when>
        <xsl:when test="value/token/@text = '`' and value/token/@type = 'special'">
          <xsl:for-each select="$next">
            <xsl:variable name="inner">
              <xsl:variable name="ctx">
                <xsl:sequence select="tokens"/>
              </xsl:variable>
              <xsl:for-each select="$ctx">
                <xsl:call-template name="malreader-read_form"/>
              </xsl:for-each>
            </xsl:variable>
            <xsl:for-each select="$inner">
              <value>
                <malval kind="list">
                  <lvalue>
                    <malval kind="symbol" value="quasiquote"/>
                    <xsl:sequence select="/value/malval"/>
                  </lvalue>
                </malval>
              </value>
              <xsl:sequence select="tokens"/>
              <xsl:sequence select="error"/>
            </xsl:for-each>
            <xsl:sequence select="lvalue"/>
            <!-- preserve previous list (if any) -->
          </xsl:for-each>
        </xsl:when>
        <xsl:when test="value/token/@text = '~' and value/token/@type = 'special'">
          <xsl:for-each select="$next">
            <xsl:variable name="inner">
              <xsl:variable name="ctx">
                <xsl:sequence select="tokens"/>
              </xsl:variable>
              <xsl:for-each select="$ctx">
                <xsl:call-template name="malreader-read_form"/>
              </xsl:for-each>
            </xsl:variable>
            <xsl:for-each select="$inner">
              <value>
                <malval kind="list">
                  <lvalue>
                    <malval kind="symbol" value="unquote"/>
                    <xsl:sequence select="/value/malval"/>
                  </lvalue>
                </malval>
              </value>
              <xsl:sequence select="tokens"/>
              <xsl:sequence select="error"/>
            </xsl:for-each>
            <xsl:sequence select="lvalue"/>
            <!-- preserve previous list (if any) -->
          </xsl:for-each>
        </xsl:when>
        <xsl:when test="value/token/@text = '~@' and value/token/@type = 'special'">
          <xsl:for-each select="$next">
            <xsl:variable name="inner">
              <xsl:variable name="ctx">
                <xsl:sequence select="tokens"/>
              </xsl:variable>
              <xsl:for-each select="$ctx">
                <xsl:call-template name="malreader-read_form"/>
              </xsl:for-each>
            </xsl:variable>
            <xsl:for-each select="$inner">
              <value>
                <malval kind="list">
                  <lvalue>
                    <malval kind="symbol" value="splice-unquote"/>
                    <xsl:sequence select="/value/malval"/>
                  </lvalue>
                </malval>
              </value>
              <xsl:sequence select="tokens"/>
              <xsl:sequence select="error"/>
            </xsl:for-each>
            <xsl:sequence select="lvalue"/>
            <!-- preserve previous list (if any) -->
          </xsl:for-each>
        </xsl:when>
        <xsl:when test="value/token/@text = '@' and value/token/@type = 'special'">
          <xsl:for-each select="$next">
            <xsl:variable name="inner">
              <xsl:variable name="ctx">
                <xsl:sequence select="tokens"/>
              </xsl:variable>
              <xsl:for-each select="$ctx">
                <xsl:call-template name="malreader-read_form"/>
              </xsl:for-each>
            </xsl:variable>
            <xsl:for-each select="$inner">
              <value>
                <malval kind="list">
                  <lvalue>
                    <malval kind="symbol" value="deref"/>
                    <xsl:sequence select="/value/malval"/>
                  </lvalue>
                </malval>
              </value>
              <xsl:sequence select="tokens"/>
              <xsl:sequence select="error"/>
            </xsl:for-each>
            <xsl:sequence select="lvalue"/>
            <!-- preserve previous list (if any) -->
          </xsl:for-each>
        </xsl:when>
        <xsl:when test="value/token/@text = '^' and value/token/@type = 'special'">
          <xsl:for-each select="$next">
            <xsl:variable name="meta">
              <xsl:variable name="ctx">
                <xsl:sequence select="tokens"/>
              </xsl:variable>
              <xsl:for-each select="$ctx">
                <xsl:call-template name="malreader-read_form"/>
              </xsl:for-each>
            </xsl:variable>
            <xsl:variable name="form">
              <xsl:for-each select="$meta">
                <xsl:variable name="ctx">
                  <xsl:sequence select="tokens"/>
                </xsl:variable>
                <xsl:for-each select="$ctx">
                  <xsl:call-template name="malreader-read_form"/>
                </xsl:for-each>
              </xsl:for-each>
            </xsl:variable>
            <xsl:for-each select="$form">
              <value>
                <malval kind="list">
                  <lvalue>
                    <malval kind="symbol" value="with-meta"/>
                    <xsl:sequence select="/value/malval"/>
                    <xsl:for-each select="$meta">
                      <xsl:sequence select="/value/malval"/>
                    </xsl:for-each>
                  </lvalue>
                </malval>
              </value>
              <xsl:sequence select="tokens"/>
              <xsl:sequence select="error"/>
            </xsl:for-each>
            <xsl:sequence select="lvalue"/>
            <!-- preserve previous list (if any) -->
          </xsl:for-each>
        </xsl:when>
        <xsl:otherwise>
          <xsl:call-template name="malreader-read_atom"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>
  <xsl:template name="malreader-read_list">
    <xsl:param name="listkind" as="xs:string"/>
    <xsl:variable name="prev_lvalue">
      <xsl:copy-of select="lvalue"/>
    </xsl:variable>
    <xsl:variable name="value">
      <xsl:variable name="ctx">
        <xsl:sequence select="tokens"/>
      </xsl:variable>
      <xsl:for-each select="$ctx">
        <xsl:call-template name="malreader-read_list_helper">
          <xsl:with-param name="listkind" select="$listkind"/>
        </xsl:call-template>
      </xsl:for-each>
    </xsl:variable>
    <xsl:for-each select="$value">
      <xsl:sequence select="tokens"/>
      <xsl:variable name="value">
        <value>
          <malval kind="{fn:list-kind($listkind)}">
            <xsl:sequence select="lvalue[1]"/>
          </malval>
        </value>
      </xsl:variable>
      <xsl:choose>
        <xsl:when test="$listkind = '{'">
          <xsl:choose>
            <xsl:when test="count($value/value/malval/lvalue/malval) mod 2 = 1">
              <error>
                <malval kind="error">Odd number of values to hash</malval>
              </error>
            </xsl:when>
            <xsl:otherwise>
              <xsl:sequence select="$value"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:when>
        <xsl:otherwise>
          <xsl:sequence select="$value"/>
          <xsl:sequence select="error"/>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:sequence select="$prev_lvalue"/>
    </xsl:for-each>
  </xsl:template>
  <xsl:template name="malreader-read_list_helper">
    <xsl:param name="listkind" as="xs:string"/>
    <xsl:choose>
      <xsl:when test="count(tokens/*) &gt; 0">
        <xsl:variable name="peek">
          <xsl:call-template name="malreader-peek"/>
        </xsl:variable>
        <xsl:for-each select="$peek">
          <xsl:choose>
            <xsl:when test="value/token/@text = fn:list-ender($listkind) and value/token/@type = 'special'">
              <!-- ok -->
              <xsl:variable name="next">
                <xsl:call-template name="malreader-next"/>
              </xsl:variable>
              <xsl:sequence select="lvalue"/>
              <xsl:sequence select="$next/tokens"/>
              <xsl:sequence select="error"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:variable name="form">
                <xsl:call-template name="malreader-read_form"/>
              </xsl:variable>
              <xsl:variable name="context">
                <xsl:for-each select="$form">
                  <!-- <xsl:message>READ_FORM <xsl:sequence select=".">
                                  
                                </xsl:sequence>
                            
                            ;</xsl:message> -->
                  <xsl:sequence select="tokens"/>
                  <lvalue>
                    <xsl:for-each select="lvalue/malval">
                      <xsl:sequence select="."/>
                    </xsl:for-each>
                    <xsl:for-each select="value/malval">
                      <xsl:sequence select="."/>
                    </xsl:for-each>
                  </lvalue>
                </xsl:for-each>
              </xsl:variable>
              <xsl:for-each select="$context">
                <xsl:call-template name="malreader-read_list_helper">
                  <xsl:with-param name="listkind" select="$listkind"/>
                </xsl:call-template>
              </xsl:for-each>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:sequence select="lvalue"/>
        </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
        <error>
          <malval kind="error">EOF while reading list</malval>
        </error>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="malreader-read_atom">
    <xsl:variable name="next">
      <xsl:call-template name="malreader-next"/>
    </xsl:variable>
    <xsl:for-each select="$next">
      <xsl:sequence select="tokens"/>
      <xsl:sequence select="lvalue"/>
      <xsl:sequence select="error"/>
      <xsl:choose>
        <xsl:when test="value/token/@type = 'number'">
          <value>
            <malval kind="number" value="{value/token/@text}"/>
          </value>
        </xsl:when>
        <xsl:when test="value/token/@type = 'symbol'">
          <value>
            <malval kind="symbol" value="{value/token/@text}"/>
          </value>
        </xsl:when>
        <xsl:when test="value/token/@type = 'string'">
          <value>
            <malval kind="string" value="{value/token/@text}"/>
          </value>
        </xsl:when>
        <xsl:when test="value/token/@type = 'keyword'">
          <value>
            <malval kind="keyword" value="{value/token/@text}"/>
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
          <error>
            <malval kind="error">
              <xsl:sequence select="value"/>
            </malval>
          </error>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>
  <xsl:function name="fn:check_string" as="xs:boolean">
    <xsl:param name="str" as="xs:string"/>
    <xsl:sequence select="$str = '&quot;' or matches($str, '[^\\](\\[^n\\&quot;]|\\(\\\\)*&quot;$)')"/>
  </xsl:function>
  <xsl:function name="fn:process-string" as="xs:string">
    <xsl:param name="str" as="xs:string"/>
    <xsl:sequence select="replace(replace($str, '([^\\]|^)\\n', '$1&#10;'), '\\([\\&quot;])', '$1')"/>
  </xsl:function>
  <xsl:function name="fn:list-ender" as="xs:string">
    <xsl:param name="str" as="xs:string"/>
    <xsl:choose>
      <xsl:when test="$str = '('">
        <xsl:sequence select="')'"/>
      </xsl:when>
      <xsl:when test="$str = '['">
        <xsl:sequence select="']'"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="'}'"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <xsl:function name="fn:list-kind" as="xs:string">
    <xsl:param name="str" as="xs:string"/>
    <xsl:choose>
      <xsl:when test="$str = '('">
        <xsl:sequence select="'list'"/>
      </xsl:when>
      <xsl:when test="$str = '['">
        <xsl:sequence select="'vector'"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="'hash'"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
</xsl:stylesheet>
