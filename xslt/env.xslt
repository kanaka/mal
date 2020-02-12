<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="3.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:fn="http://www.w3.org/2005/02/xpath-functions"  xmlns:xs="http://www.w3.org/2001/XMLSchema"  xmlns:map="http://www.w3.org/2005/xpath-functions/map" xmlns:env="ENV">
    <!-- since I can not, for the life of me, figure out how to (de-)serialise maps from/to xml, we're gonna be storing the env as a json string -->
    
    <xsl:function name="env:set">
        <xsl:param name="env"/>
        <xsl:param name="name"/>
        <xsl:param name="value"/>
        <xsl:sequence select="map { 'outer': $env('outer'), 'data': map:put(map:merge($env('data')), $name, $value => serialize(map{})) }"/>
    </xsl:function>

    <xsl:function name="env:find">
        <xsl:param name="env"/>
        <xsl:param name="name"/>
        <xsl:sequence select="if (empty($env)) then () else if (map:contains($env('data'), $name)) then $env else env:find($env('outer'), $name)"/>
    </xsl:function>

    <xsl:function name="env:get">
        <xsl:param name="env"/>
        <xsl:param name="name"/>
        <xsl:variable name="value" select="let $venv := env:find($env, $name) return if (empty($venv)) then () else $venv('data')($name)"></xsl:variable>
        <xsl:choose>
            <xsl:when test="empty($value)"><xsl:message terminate="yes">Symbol <xsl:value-of select="$name" /> not found</xsl:message></xsl:when>
            <xsl:otherwise><xsl:sequence select="parse-xml($value)"/></xsl:otherwise>
        </xsl:choose>
    </xsl:function>

    <xsl:function name="env:base" as="xs:string">
        <xsl:variable name="plus"><malval kind="function" name="+"></malval></xsl:variable>
        <xsl:variable name="minus"><malval kind="function" name="-"></malval></xsl:variable>
        <xsl:variable name="mult"><malval kind="function" name="*"></malval></xsl:variable>
        <xsl:variable name="div"><malval kind="function" name="/"></malval></xsl:variable>
        <xsl:sequence select="env:serialise(env:set(env:set(env:set(env:set(map{'outer':(), 'data':map{}}, '+', $plus), '-', $minus), '*', $mult), '/', $div))"/>
    </xsl:function>

    <xsl:function name="env:empty">
        <xsl:sequence select="map{'outer':(), 'data':map{}}"/>
    </xsl:function>

    <xsl:function name="env:serialise">
        <xsl:param name="env"/>
        <xsl:sequence select="serialize($env, map {'method': 'json'})"/>
    </xsl:function>

    <xsl:function name="env:close">
        <xsl:param name="env"/>
        <xsl:sequence select="map {'outer': $env, 'data': map{}}"/>
    </xsl:function>

    <xsl:function name="env:close-with-binds">
        <xsl:param name="env"/>
        <xsl:param name="binds" />
        <xsl:param name="exprs" />

        <xsl:variable name="new-env" select="map {'outer': $env, 'data': map{}}"/>
        <xsl:sequence select="$new-env => env:bind-all($binds, $exprs)"/>
    </xsl:function>
    
    <xsl:function name="env:dump">
        <xsl:param name="env"/>
        <xsl:sequence select="if (not(empty($env))) then map:merge(($env('data'), env:dump($env('outer')))) else map{}"/>
    </xsl:function>

    <xsl:function name="env:merge">
        <xsl:param name="env" />
        <xsl:param name="second" />

        <xsl:variable name="env-items" select="env:dump($env)"></xsl:variable>
        <xsl:variable name="second-items" select="env:dump($second)"></xsl:variable>
        <xsl:variable name="new-env" select="map {'outer': (), 'data': map:merge(($env-items, $second-items))}"></xsl:variable>
        <xsl:sequence select="$new-env"/> 
    </xsl:function>

    <xsl:function name="env:hier">
        <xsl:param name="env"/>
        <xsl:param name="over"/>
        <xsl:sequence select="map{'outer': env:merge($over, $env), 'data': map{}}"/>
    </xsl:function>

    <xsl:function name="env:bind-all">
        <xsl:param name="env"/>
        <xsl:param name="binds"/>
        <xsl:param name="exprs"/>
        <xsl:choose>
            <xsl:when test="exists($binds) and exists($exprs)">
                <xsl:choose>
                    <xsl:when test="string(head($binds)) = '&#38;'">
                        <xsl:variable name="listExprs">
                            <malval kind="list">
                                <lvalue>
                                    <xsl:sequence select="$exprs"/>
                                </lvalue>
                            </malval>
                        </xsl:variable>
                        <xsl:sequence select="$env => env:set(string($binds[2]), $listExprs)"/>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:variable name="new-env" select="$env => env:set(string(head($binds)), head($exprs))"></xsl:variable>
                        <xsl:sequence select="$new-env => env:bind-all(tail($binds), tail($exprs))"/>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:when>
            <xsl:when test="exists($binds)">
              <xsl:choose>
                  <xsl:when test="string(head($binds)) = '&#38;'">
                        <xsl:variable name="listExprs">
                            <malval kind="list">
                                <lvalue>
                                </lvalue>
                            </malval>
                        </xsl:variable>
                        <xsl:sequence select="$env => env:set(string($binds[2]), $listExprs)"/>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:variable name="Exprs">
                            <malval kind="nil">
                            </malval>
                        </xsl:variable>
                        <xsl:variable name="new-env" select="$env => env:set(string(head($binds)), $Exprs)"></xsl:variable>
                        <xsl:sequence select="$new-env => env:bind-all(tail($binds), $exprs)"/>
                    </xsl:otherwise>
              </xsl:choose>
            </xsl:when>
            <xsl:otherwise>
                <xsl:sequence select="$env"/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:function>

    <xsl:function name="env:deserialise">
        <xsl:param name="env"/>
        <xsl:sequence select="parse-json($env)"/>
    </xsl:function>

</xsl:stylesheet>
