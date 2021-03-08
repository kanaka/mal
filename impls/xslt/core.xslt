<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:fn="http://www.w3.org/2005/02/xpath-functions" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:core="CORE" version="3.0" exclude-result-prefixes="core fn xsl xs">
  <xsl:function name="core:ns">
    <xsl:sequence>
      <malval kind="function" name="+">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="-">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="*">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="/">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="prn">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="pr-str">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="str">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="println">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="list">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="list?">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="empty?">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="count">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="=">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="&lt;">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="&lt;=">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="&gt;">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="&gt;=">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="read-string">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="slurp">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="eval">
        <is_macro>false</is_macro>
      </malval>
      <!-- defined in the step files -->
      <malval kind="function" name="atom">
        <is_macro>false</is_macro>
      </malval>
      <!-- defined in the step files -->
      <malval kind="function" name="atom?">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="deref">
        <is_macro>false</is_macro>
      </malval>
      <!-- defined in the step files -->
      <malval kind="function" name="swap!">
        <is_macro>false</is_macro>
      </malval>
      <!-- defined in the step files -->
      <malval kind="function" name="reset!">
        <is_macro>false</is_macro>
      </malval>
      <!-- defined in the step files -->
      <malval kind="function" name="cons">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="concat">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="vec">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="nth">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="first">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="rest">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="throw">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="apply">
        <is_macro>false</is_macro>
      </malval>
      <!-- defined in the step files -->
      <malval kind="function" name="map">
        <is_macro>false</is_macro>
      </malval>
      <!-- defined in the step files -->
      <malval kind="function" name="nil?">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="true?">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="false?">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="symbol?">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="symbol">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="keyword">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="keyword?">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="vector">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="vector?">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="sequential?">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="hash-map">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="map?">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="assoc">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="dissoc">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="get">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="contains?">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="keys">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="vals">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="readline">
        <is_macro>false</is_macro>
      </malval>
      <!-- defined in step file -->
      <malval kind="function" name="meta">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="with-meta">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="time-ms">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="conj">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="string?">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="number?">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="fn?">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="macro?">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="seq">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="xpath-eval">
        <is_macro>false</is_macro>
      </malval>
      <malval kind="function" name="xslt-halt">
        <is_macro>false</is_macro>
      </malval>
      <!-- evaluate xpath, no context node | requires Saxon PE/EE [paywalls piss me off] -->
    </xsl:sequence>
  </xsl:function>
  <xsl:template name="core-apply">
    <xsl:param name="func"/>
    <xsl:param name="args"/>
    <xsl:variable name="atoms" select="atoms"/>
    <xsl:choose>
      <xsl:when test="$func/malval/@kind = 'function'">
        <xsl:choose>
          <xsl:when test="$func/malval/@name = '+'">
            <xsl:variable name="result" select="number($args/value/malval/lvalue/malval[1]/@value) + number($args/value/malval/lvalue/malval[2]/@value)"/>
            <xsl:sequence select="core:makeMALType($result, 'number')"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = '-'">
            <xsl:variable name="result" select="number($args/value/malval/lvalue/malval[1]/@value) - number($args/value/malval/lvalue/malval[2]/@value)"/>
            <xsl:sequence select="core:makeMALType($result, 'number')"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = '*'">
            <xsl:variable name="result" select="number($args/value/malval/lvalue/malval[1]/@value) * number($args/value/malval/lvalue/malval[2]/@value)"/>
            <xsl:sequence select="core:makeMALType($result, 'number')"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = '/'">
            <xsl:variable name="result" select="number($args/value/malval/lvalue/malval[1]/@value) div number($args/value/malval/lvalue/malval[2]/@value)"/>
            <xsl:sequence select="core:makeMALType($result, 'number')"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'prn'">
            <xsl:variable name="args" select="$args/value/malval/lvalue/malval"/>
            <xsl:variable name="sargs">
              <xsl:for-each select="$args">
                <xsl:variable name="arg">
                  <value>
                    <xsl:sequence select="."/>
                  </value>
                  <xsl:sequence select="$atoms"/>
                </xsl:variable>
                <str>
                  <xsl:for-each select="$arg">
                    <xsl:call-template name="malprinter-pr_str">
                      <xsl:with-param name="readably" select="true()"/>
                    </xsl:call-template>
                  </xsl:for-each>
                </str>
              </xsl:for-each>
            </xsl:variable>
            <xsl:message>
              <request kind="display" value="{string-join($sargs/str, ' ')}"/>
            </xsl:message>
            <xsl:sequence select="core:makeMALType((), 'nil')"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'pr-str'">
            <xsl:variable name="args" select="$args/value/malval/lvalue/malval"/>
            <xsl:variable name="sargs">
              <xsl:for-each select="$args">
                <xsl:variable name="arg">
                  <value>
                    <xsl:sequence select="."/>
                  </value>
                  <xsl:sequence select="$atoms"/>
                </xsl:variable>
                <str>
                  <xsl:for-each select="$arg">
                    <xsl:call-template name="malprinter-pr_str">
                      <xsl:with-param name="readably" select="true()"/>
                    </xsl:call-template>
                  </xsl:for-each>
                </str>
              </xsl:for-each>
            </xsl:variable>
            <xsl:sequence select="core:makeMALType(string-join($sargs/str, ' '), 'string')"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'str'">
            <xsl:variable name="args" select="$args/value/malval/lvalue/malval"/>
            <xsl:variable name="sargs">
              <xsl:for-each select="$args">
                <xsl:variable name="arg">
                  <value>
                    <xsl:sequence select="."/>
                  </value>
                  <xsl:sequence select="$atoms"/>
                </xsl:variable>
                <str>
                  <xsl:for-each select="$arg">
                    <xsl:call-template name="malprinter-pr_str">
                      <xsl:with-param name="readably" select="false()"/>
                    </xsl:call-template>
                  </xsl:for-each>
                </str>
              </xsl:for-each>
            </xsl:variable>
            <xsl:sequence select="core:makeMALType(string-join($sargs/str, ''), 'string')"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'println'">
            <xsl:variable name="args" select="$args/value/malval/lvalue/malval"/>
            <xsl:variable name="sargs">
              <xsl:for-each select="$args">
                <xsl:variable name="arg">
                  <value>
                    <xsl:sequence select="."/>
                  </value>
                  <xsl:sequence select="$atoms"/>
                </xsl:variable>
                <str>
                  <xsl:for-each select="$arg">
                    <xsl:call-template name="malprinter-pr_str">
                      <xsl:with-param name="readably" select="false()"/>
                    </xsl:call-template>
                  </xsl:for-each>
                </str>
              </xsl:for-each>
            </xsl:variable>
            <xsl:message>
              <request kind="display" value="{string-join($sargs/str, ' ')}"/>
            </xsl:message>
            <xsl:sequence select="core:makeMALType((), 'nil')"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'list'">
            <xsl:sequence select="$args"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'list?'">
            <xsl:sequence select="core:makeMALType((), if ($args/value/malval/lvalue/malval[1]/@kind = 'list') then 'true' else 'false')"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'empty?'">
            <xsl:sequence select="core:makeMALType((), if (count($args/value/malval/lvalue/malval[1]/lvalue/malval) = 0) then 'true' else 'false')"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'count'">
            <xsl:choose>
              <xsl:when test="$args/value/malval/lvalue/malval[1]/@kind = 'hash'">
                <xsl:sequence select="core:makeMALType(count($args/value/malval/lvalue/malval[1]/lvalue/malval) div 2, 'number')"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:sequence select="core:makeMALType(count($args/value/malval/lvalue/malval[1]/lvalue/malval), 'number')"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:when>
          <xsl:when test="$func/malval/@name = '='">
            <xsl:sequence select="core:makeMALType((), if (core:equal($args/value/malval/lvalue/malval[1], $args/value/malval/lvalue/malval[2])) then 'true' else 'false')"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = '&lt;'">
            <xsl:sequence select="core:makeMALType((), if (number($args/value/malval/lvalue/malval[1]/@value) lt number($args/value/malval/lvalue/malval[2]/@value)) then 'true' else 'false')"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = '&lt;='">
            <xsl:sequence select="core:makeMALType((), if (number($args/value/malval/lvalue/malval[1]/@value) le number($args/value/malval/lvalue/malval[2]/@value)) then 'true' else 'false')"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = '&gt;'">
            <xsl:sequence select="core:makeMALType((), if (number($args/value/malval/lvalue/malval[1]/@value) gt number($args/value/malval/lvalue/malval[2]/@value)) then 'true' else 'false')"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = '&gt;='">
            <xsl:sequence select="core:makeMALType((), if (number($args/value/malval/lvalue/malval[1]/@value) ge number($args/value/malval/lvalue/malval[2]/@value)) then 'true' else 'false')"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'read-string'">
            <xsl:variable name="read-string-context">
              <str>
                <xsl:value-of select="$args/value/malval/lvalue/malval[1]/@value"/>
              </str>
            </xsl:variable>
            <xsl:for-each select="$read-string-context">
              <xsl:call-template name="malreader-read_str"/>
            </xsl:for-each>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'slurp'">
            <xsl:sequence select="core:makeMALType(unparsed-text($args/value/malval/lvalue/malval[1]/@value), 'string')"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'atom?'">
            <xsl:sequence select="core:makeMALType((), if ($args/value/malval/lvalue/malval[1]/@kind = 'atom') then 'true' else 'false')"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'cons'">
            <xsl:variable name="result">
              <value>
                <malval kind="list">
                  <lvalue>
                    <xsl:sequence select="$args/value/malval/lvalue/malval[1]"/>
                    <xsl:sequence select="$args/value/malval/lvalue/malval[2]/lvalue/malval"/>
                  </lvalue>
                </malval>
              </value>
            </xsl:variable>
            <xsl:sequence select="$result"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'concat'">
            <xsl:variable name="result">
              <value>
                <malval kind="list">
                  <lvalue>
                    <xsl:sequence select="$args/value/malval/lvalue/malval/lvalue/malval"/>
                  </lvalue>
                </malval>
              </value>
            </xsl:variable>
            <xsl:sequence select="$result"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'vec'">
            <value>
              <malval kind="vector">
                <xsl:sequence select="$args/value/malval/lvalue/malval[1]/lvalue"/>
              </malval>
            </value>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'nth'">
            <value>
              <xsl:variable name="res" select="$args/value/malval/lvalue/malval[1]/lvalue/malval[position() = (number($args/value/malval/lvalue/malval[2]/@value) + 1)]"/>
              <xsl:if test="empty($res)">
                <xsl:value-of select="error(QName('MAL', 'Error'), 'Index out of bounds', core:makeMALValue('Index out of bounds', 'string'))"/>
              </xsl:if>
              <xsl:sequence select="$res"/>
            </value>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'first'">
            <value>
              <xsl:variable name="res" select="$args/value/malval/lvalue/malval[1]/lvalue/malval[1]"/>
              <xsl:choose>
                <xsl:when test="empty($res)">
                  <malval kind="nil"/>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:sequence select="$res"/>
                </xsl:otherwise>
              </xsl:choose>
            </value>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'rest'">
            <value>
              <malval kind="list">
                <lvalue>
                  <xsl:sequence select="$args/value/malval/lvalue/malval[1]/lvalue/malval[position() &gt; 1]"/>
                </lvalue>
              </malval>
            </value>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'throw'">
            <xsl:variable name="err" select="$args/value/malval/lvalue/malval[1]"/>
            <xsl:value-of select="error(QName('MAL', 'Error'), core:pr-str($err), $err)"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'nil?'">
            <xsl:sequence select="core:makeMALType((), if ($args/value/malval/lvalue/malval[1]/@kind = 'nil') then 'true' else 'false')"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'true?'">
            <xsl:sequence select="core:makeMALType((), if ($args/value/malval/lvalue/malval[1]/@kind = 'true') then 'true' else 'false')"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'false?'">
            <xsl:sequence select="core:makeMALType((), if ($args/value/malval/lvalue/malval[1]/@kind = 'false') then 'true' else 'false')"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'symbol?'">
            <xsl:sequence select="core:makeMALType((), if ($args/value/malval/lvalue/malval[1]/@kind = 'symbol') then 'true' else 'false')"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'symbol'">
            <xsl:sequence select="core:makeMALType($args/value/malval/lvalue/malval[1]/@value, 'symbol')"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'keyword'">
            <xsl:sequence select="core:makeMALType($args/value/malval/lvalue/malval[1]/@value, 'keyword')"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'keyword?'">
            <xsl:sequence select="core:makeMALType((), if ($args/value/malval/lvalue/malval[1]/@kind = 'keyword') then 'true' else 'false')"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'vector'">
            <value>
              <malval kind="vector">
                <xsl:sequence select="$args/value/malval/lvalue"/>
              </malval>
            </value>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'vector?'">
            <xsl:sequence select="core:makeMALType((), if ($args/value/malval/lvalue/malval[1]/@kind = 'vector') then 'true' else 'false')"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'sequential?'">
            <xsl:sequence select="core:makeMALType((), if (let $kind := $args/value/malval/lvalue/malval[1]/@kind return $kind = 'vector' or $kind = 'list') then 'true' else 'false')"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'hash-map'">
            <xsl:if test="count($args/value/malval/lvalue/malval) mod 2 = 1">
              <xsl:value-of select="error(QName('MAL', 'Error'), 'Odd number of args to hash-map', core:makeMALValue('Odd number of args to hash-map', 'string'))"/>
            </xsl:if>
            <value>
              <malval kind="hash">
                <xsl:sequence select="$args/value/malval/lvalue"/>
              </malval>
            </value>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'map?'">
            <xsl:sequence select="core:makeMALType((), if ($args/value/malval/lvalue/malval[1]/@kind = 'hash') then 'true' else 'false')"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'assoc'">
            <xsl:if test="count($args/value/malval/lvalue/malval) mod 2 = 0">
              <xsl:value-of select="error(QName('MAL', 'Error'), 'Odd number of args to assoc', core:makeMALValue('Odd number of args to assoc', 'string'))"/>
            </xsl:if>
            <xsl:variable name="names" select="$args/value/malval/lvalue/malval[(position() gt 1) and (position() mod 2 = 0)]"/>
            <xsl:variable name="values" select="$args/value/malval/lvalue/malval[(position() gt 2) and (position() mod 2 = 1)]"/>
            <xsl:sequence select="let $hash := $args/value/malval/lvalue/malval[1] return core:map-dissoc($hash/lvalue/malval, $names) =&gt; core:map-assoc($names, $values) =&gt; core:makeMALList('hash')"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'dissoc'">
            <xsl:variable name="names" select="$args/value/malval/lvalue/malval[position() gt 1]"/>
            <xsl:sequence select="let $hash := $args/value/malval/lvalue/malval[1] return core:map-dissoc($hash/lvalue/malval, $names) =&gt; core:makeMALList('hash')"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'get'">
            <xsl:variable name="name" select="$args/value/malval/lvalue/malval[2]"/>
            <value>
              <xsl:sequence select="let $hash := $args/value/malval/lvalue/malval[1] return (core:map-get($hash/lvalue/malval, $name), core:makeMALValue((), 'nil'))[1]"/>
            </value>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'contains?'">
            <xsl:variable name="name" select="$args/value/malval/lvalue/malval[2]"/>
            <xsl:sequence select="let $hash := $args/value/malval/lvalue/malval[1] return core:makeMALType((), if (empty(core:map-get($hash/lvalue/malval, $name))) then 'false' else 'true')"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'keys'">
            <xsl:sequence select="let $hash := $args/value/malval/lvalue/malval[1] return ($hash/lvalue/malval[position() mod 2 = 1]) =&gt; core:makeMALList('list')"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'vals'">
            <xsl:sequence select="let $hash := $args/value/malval/lvalue/malval[1] return ($hash/lvalue/malval[position() mod 2 = 0]) =&gt; core:makeMALList('list')"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'meta'">
            <value>
              <xsl:sequence select="($args/value/malval/lvalue/malval[1]/meta/malval, core:makeMALValue((), 'nil'))[1]"/>
            </value>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'with-meta'">
            <value>
              <malval>
                <xsl:sequence select="$args/value/malval/lvalue/malval[1]/(*[name() != 'meta']|@*)"/>
                <meta>
                  <xsl:sequence select="$args/value/malval/lvalue/malval[2]"/>
                </meta>
              </malval>
            </value>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'time-ms'">
            <!-- current-dateTime() does not change while transforming :( -->
            <xsl:sequence select="core:makeMALType(core:mstime(), 'number')"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'conj'">
            <xsl:variable name="xargs" select="$args/value/malval/lvalue/malval[position() &gt; 1]"/>
            <xsl:variable name="coll">
              <xsl:sequence select="$args/value/malval/lvalue/malval[1]"/>
            </xsl:variable>
            <value>
              <xsl:choose>
                <xsl:when test="$coll/malval/@kind = 'list'">
                  <malval kind="list">
                    <lvalue>
                      <xsl:sequence select="reverse($xargs)"/>
                      <xsl:sequence select="$coll/malval/lvalue/malval"/>
                    </lvalue>
                  </malval>
                </xsl:when>
                <xsl:otherwise>
                  <malval kind="vector">
                    <lvalue>
                      <xsl:sequence select="$coll/malval/lvalue/malval"/>
                      <xsl:sequence select="$xargs"/>
                    </lvalue>
                  </malval>
                </xsl:otherwise>
              </xsl:choose>
            </value>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'string?'">
            <xsl:sequence select="core:makeMALType((), if ($args/value/malval/lvalue/malval[1]/@kind = 'string') then 'true' else 'false')"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'number?'">
            <xsl:sequence select="core:makeMALType((), if ($args/value/malval/lvalue/malval[1]/@kind = 'number') then 'true' else 'false')"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'fn?'">
            <xsl:sequence select="core:makeMALType((), if (let $f := $args/value/malval/lvalue/malval[1] return ($f/@kind = 'userfunction' or $f/@kind = 'function') and $f/is_macro/text() != 'true') then 'true' else 'false')"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'macro?'">
            <xsl:sequence select="core:makeMALType((), if (let $f := $args/value/malval/lvalue/malval[1] return ($f/@kind = 'userfunction' or $f/@kind = 'function') and $f/is_macro/text() = 'true') then 'true' else 'false')"/>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'seq'">
            <xsl:variable name="arg" select="$args/value/malval/lvalue/malval[1]"/>
            <xsl:choose>
              <xsl:when test="$arg/@kind = 'string'">
                <xsl:choose>
                  <xsl:when test="string-length($arg/@value) = 0">
                    <xsl:sequence select="core:makeMALType((), 'nil')"/>
                  </xsl:when>
                  <xsl:otherwise>
                    <value>
                      <malval kind="list">
                        <lvalue>
                          <xsl:for-each select="string-to-codepoints($arg/@value)">
                            <xsl:sequence select="core:makeMALValue(codepoints-to-string(.), 'string')"/>
                          </xsl:for-each>
                        </lvalue>
                      </malval>
                    </value>
                  </xsl:otherwise>
                </xsl:choose>
              </xsl:when>
              <xsl:otherwise>
                <xsl:choose>
                  <xsl:when test="count($arg/lvalue/malval) = 0">
                    <xsl:sequence select="core:makeMALType((), 'nil')"/>
                  </xsl:when>
                  <xsl:otherwise>
                    <value>
                      <malval kind="list">
                        <xsl:sequence select="$arg/lvalue"/>
                      </malval>
                    </value>
                  </xsl:otherwise>
                </xsl:choose>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'xpath-eval'">
            <xsl:message>
              <request kind="xpath-eval" value="{$args/value/malval/lvalue/malval[1]/@value}" context="{$args/value/malval/lvalue/malval[2] =&gt; serialize()}"/>
            </xsl:message>
            <value>
              <xsl:sequence select="document('xsl_input-string')"/>
            </value>
          </xsl:when>
          <xsl:when test="$func/malval/@name = 'xslt-halt'">
            <xsl:message>
              <request kind="halt"/>
            </xsl:message>
            <value>
              <malval kind="list" />
            </value>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="error(QName('MAL', 'Error'), concat('Invalid function ', $func/malval/@name), core:makeMALValue(concat('Invalid function ', $func/malval/@name), 'string'))"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:otherwise/>
    </xsl:choose>
  </xsl:template>
  <xsl:function name="core:makeMALList">
    <xsl:param name="values"/>
    <xsl:param name="kind"/>
    <value>
      <malval kind="{$kind}">
        <lvalue>
          <xsl:sequence select="$values"/>
        </lvalue>
      </malval>
    </value>
  </xsl:function>
  <xsl:function name="core:makeMALType">
    <xsl:param name="value"/>
    <xsl:param name="kind"/>
    <value>
      <malval kind="{$kind}" value="{$value}"/>
    </value>
  </xsl:function>
  <xsl:function name="core:makeMALValue">
    <xsl:param name="value"/>
    <xsl:param name="kind"/>
    <malval kind="{$kind}" value="{$value}"/>
  </xsl:function>
  <xsl:function name="core:pr-str">
    <xsl:param name="value"/>
    <xsl:variable name="ctx">
      <value>
        <xsl:sequence select="$value"/>
      </value>
    </xsl:variable>
    <xsl:variable name="res">
      <xsl:for-each select="$ctx">
        <xsl:call-template name="malprinter-pr_str">
          <xsl:with-param name="readably" select="true()"/>
        </xsl:call-template>
      </xsl:for-each>
    </xsl:variable>
    <xsl:sequence select="$res"/>
  </xsl:function>
  <xsl:function name="core:all-equal">
    <xsl:param name="seq"/>
    <xsl:param name="left"/>
    <xsl:param name="right"/>
    <xsl:choose>
      <xsl:when test="empty($seq)">
        <xsl:sequence select="true()"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:choose>
          <xsl:when test="core:list-equal($left, $right, head($seq))">
            <xsl:sequence select="core:all-equal(tail($seq), $right, $left)"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:sequence select="false()"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <xsl:function name="core:any-equal">
    <xsl:param name="comps"/>
    <xsl:param name="value"/>
    <xsl:choose>
      <xsl:when test="empty($comps)">
        <xsl:sequence select="false()"/>
      </xsl:when>
      <xsl:when test="core:equal($value, head($comps))">
        <xsl:sequence select="true()"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="core:any-equal(tail($comps), $value)"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <xsl:function name="core:map-values-equal">
    <xsl:param name="ma"/>
    <xsl:param name="mb"/>
    <xsl:choose>
      <xsl:when test="empty($ma)">
        <xsl:sequence select="true()"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="core:equal($ma[2], core:map-get($mb, $ma[1])) and core:map-values-equal($ma[position() gt 2], $mb)"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <xsl:function name="core:equal">
    <xsl:param name="left"/>
    <xsl:param name="right"/>
    <xsl:choose>
      <!-- equal kinds -->
      <xsl:when test="$left/@kind = $right/@kind">
        <xsl:choose>
          <xsl:when test="$left/@kind = 'hash' and $right/@kind = 'hash'">
            <!-- counts are equal, check if all keys share the same value -->
            <xsl:sequence select="core:map-values-equal($left/lvalue/malval, $right/lvalue/malval)"/>
          </xsl:when>
          <!-- sequence? -->
          <xsl:when test="$left/@kind = 'list' or $left/@kind = 'vector' or $left/@kind = 'hash'">
            <xsl:choose>
              <!-- same counts -->
              <xsl:when test="count($left/lvalue/malval) = count($right/lvalue/malval)">
                <xsl:sequence select="core:all-equal(1 to count($left/lvalue/malval), $left, $right)"/>
              </xsl:when>
              <!-- different counts -->
              <xsl:otherwise>
                <xsl:sequence select="false()"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:when>
          <!-- simple 'value' type -->
          <xsl:otherwise>
            <xsl:sequence select="string($left/@value) = string($right/@value)"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <!-- different types -->
      <xsl:otherwise>
        <xsl:choose>
          <xsl:when test="($left/@kind = 'list' and $right/@kind = 'vector') or ($left/@kind = 'vector' and $right/@kind = 'list')">
            <xsl:choose>
              <!-- same counts -->
              <xsl:when test="count($left/lvalue/malval) = count($right/lvalue/malval)">
                <xsl:sequence select="core:all-equal(1 to count($left/lvalue/malval), $left, $right)"/>
              </xsl:when>
              <!-- different counts -->
              <xsl:otherwise>
                <xsl:sequence select="false()"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:when>
          <xsl:otherwise>
            <xsl:sequence select="false()"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <xsl:function name="core:list-equal">
    <xsl:param name="l"/>
    <xsl:param name="r"/>
    <xsl:param name="v"/>
    <xsl:sequence select="core:equal($l/lvalue/malval[$v], $r/lvalue/malval[$v])"/>
  </xsl:function>
  <xsl:function name="core:map-assoc">
    <xsl:param name="map"/>
    <xsl:param name="names"/>
    <xsl:param name="values"/>
    <xsl:sequence select="$map"/>
    <xsl:for-each select="1 to count($names)">
      <xsl:variable name="idx" select="position()"/>
      <xsl:sequence select="$names[position() = $idx]"/>
      <xsl:sequence select="$values[position() = $idx]"/>
    </xsl:for-each>
  </xsl:function>
  <xsl:function name="core:map-dissoc">
    <xsl:param name="map"/>
    <xsl:param name="names"/>
    <xsl:sequence select="$map[let $pos := position() return not(($pos mod 2 = 1 and core:any-equal($names, .)) or ($pos mod 2 = 0 and core:any-equal($names, ../malval[$pos - 1])))]"/>
  </xsl:function>
  <xsl:function name="core:map-get">
    <xsl:param name="map"/>
    <xsl:param name="name"/>
    <xsl:sequence select="$map[let $pos := position() return $pos mod 2 = 0 and core:equal($name, ../malval[$pos - 1])]"/>
  </xsl:function>
  <xsl:function name="core:mstime">
    <xsl:message>
      <request kind="time" value="now"/>
    </xsl:message>
    <xsl:sequence select="unparsed-text('xsl_input-string')"/>
  </xsl:function>
</xsl:stylesheet>
