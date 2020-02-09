<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="3.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:fn="http://www.w3.org/2005/02/xpath-functions"  xmlns:xs="http://www.w3.org/2001/XMLSchema"  xmlns:map="http://www.w3.org/2005/xpath-functions/map" xmlns:env="http://www.w3.org/2005/02/xpath-functions">
    <!-- since I can not, for the life of me, figure out how to (de-)serialise maps from/to xml, we're gonna be storing the env as a json string -->
    <xsl:function name="env:set">
        <xsl:param name="env"/>
        <xsl:param name="name"/>
        <xsl:param name="value"/>
        <xsl:sequence select="map { 'outer': $env('outer'), 'data': map:put(map:merge($env('data')), $name, $value => serialize()) }"/>
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

    <xsl:function name="env:serialise">
        <xsl:param name="env"/>
        <xsl:sequence select="serialize($env, map {'method': 'json'})"/>
    </xsl:function>

    <xsl:function name="env:close">
        <xsl:param name="env"/>
        <xsl:sequence select="map {'outer': $env, 'data': map{}}"/>
    </xsl:function>

    <xsl:function name="env:deserialise">
        <xsl:param name="env"/>
        <xsl:sequence select="parse-json($env)"/>
    </xsl:function>

</xsl:stylesheet>