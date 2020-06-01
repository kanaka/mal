<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:fn="http://www.w3.org/2005/02/xpath-functions" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:map="http://www.w3.org/2005/xpath-functions/map" xmlns:env="ENV" xmlns:core="CORE" version="3.0">
  <!-- since I can not, for the life of me, figure out how to (de-)serialise maps from/to xml, we're gonna be storing the env as a json string -->
  <xsl:function name="env:noReplEnv">
    <xsl:param name="env"/>
    <xsl:sequence select="map {'outer': $env('outer'), 'isReplEnv': false(), 'data': $env('data') }"/>
  </xsl:function>
  <xsl:function name="env:set">
    <xsl:param name="env"/>
    <xsl:param name="name"/>
    <xsl:param name="value"/>
    <xsl:sequence select="if ($env('isReplEnv')) then
                            map { 'outer': $env('outer'), 'replEnv': env:set($env('replEnv'), $name, $value), 'isReplEnv': true(), 'data': $env('data') }
                          else
                            map { 'outer': $env('outer'), 'replEnv': $env('replEnv'), 'isReplEnv': false(), 'data': map:put($env('data'), $name, $value =&gt; serialize(map{})) }"/>
  </xsl:function>
  <xsl:function name="env:find">
    <xsl:param name="env"/>
    <xsl:param name="name"/>
    <xsl:sequence select="if (empty($env)) then
                            ()
                          else if (map:contains($env('data'), $name)) then
                            $env
                          else
                            (env:find($env('outer'), $name), env:find($env('replEnv'), $name))[1]"/>
  </xsl:function>
  <xsl:function name="env:get">
    <xsl:param name="env"/>
    <xsl:param name="name"/>
    <xsl:variable name="value" select="let $venv := env:find($env, $name)
                                        return if (empty($venv)) then
                                                ()
                                               else
                                                $venv('data')($name)"/>
    <xsl:choose>
      <xsl:when test="empty($value)">
        <xsl:variable name="apos" select="&quot;'&quot;"/>
        <xsl:value-of select="error(
                                QName('MAL', 'Error'),
                                concat($apos, $name, $apos, ' not found'),
                                fn:makeMALValue(concat($apos, $name, $apos, ' not found'), 'string'))"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:try>
          <xsl:sequence select="parse-xml($value)"/>
          <xsl:catch errors="*">
            <xsl:sequence select="$value"/>
          </xsl:catch>
        </xsl:try>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <xsl:function name="env:get-noerror">
    <xsl:param name="env"/>
    <xsl:param name="name"/>
    <xsl:variable name="value" select="let $venv := env:find($env, $name)
                                        return if (empty($venv)) then 
                                                ()
                                               else
                                                $venv('data')($name)"/>
    <xsl:choose>
      <xsl:when test="empty($value)"/>
      <xsl:otherwise>
        <xsl:sequence select="parse-xml($value)"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <xsl:function name="env:base" as="xs:string">
    <xsl:variable name="plus">
      <malval kind="function" name="+"/>
    </xsl:variable>
    <xsl:variable name="minus">
      <malval kind="function" name="-"/>
    </xsl:variable>
    <xsl:variable name="mult">
      <malval kind="function" name="*"/>
    </xsl:variable>
    <xsl:variable name="div">
      <malval kind="function" name="/"/>
    </xsl:variable>
    <xsl:sequence select="env:serialise(env:set(env:set(env:set(env:set(map{'outer':(), 'data':map{}, 'isReplEnv': false()}, '+', $plus), '-', $minus), '*', $mult), '/', $div))"/>
  </xsl:function>
  <xsl:function name="env:swap-replEnv">
    <xsl:param name="env"/>
    <xsl:param name="toRepl"/>
    <xsl:sequence select="if (not(empty($toRepl))) then
                            map:put($env, 'replEnv', $toRepl)
                          else
                            $env"/>
  </xsl:function>
  <xsl:function name="env:replEnv">
    <xsl:param name="env"/>
    <xsl:sequence select="$env('replEnv')"/>
  </xsl:function>
  <xsl:function name="env:toReplEnv">
    <xsl:param name="env"/>
    <xsl:variable name="renv" select="map{'outer':(), 'data': map{}, 'replEnv': map{'data':env:dump($env), 'outer':(), 'isReplEnv': false()}, 'isReplEnv': true()}"/>
    <xsl:sequence select="$renv"/>
  </xsl:function>
  <xsl:function name="env:wrapReplEnv">
    <xsl:param name="replEnv"/>
    <xsl:sequence select="map{'outer':(), 'data':map{}, 'replEnv':$replEnv, 'isReplEnv':false()}"/>
  </xsl:function>
  <xsl:function name="env:collapseReplEnv">
    <xsl:param name="env"/>
    <xsl:iterate select="map:keys($env('data'))">
      <xsl:param name="new-env" select="$env('replEnv')"/>
      <xsl:on-completion>
        <xsl:sequence select="$new-env"/>
      </xsl:on-completion>
      <xsl:variable name="name" select="."/>
      <xsl:next-iteration>
        <xsl:with-param name="new-env" select="env:set($new-env, $name, $env =&gt; env:get($name))"/>
      </xsl:next-iteration>
    </xsl:iterate>
  </xsl:function>
  <xsl:function name="env:empty">
    <xsl:sequence select="map{'outer':(), 'data':map{}, 'isReplEnv': false()}"/>
  </xsl:function>
  <xsl:function name="env:serialise">
    <xsl:param name="env"/>
    <xsl:sequence select="serialize($env, map {'method': 'json'})"/>
  </xsl:function>
  <xsl:function name="env:close">
    <xsl:param name="env"/>
    <xsl:sequence select="map{'outer': env:noReplEnv($env), 'data': map{}, 'isReplEnv': false(), 'replEnv': $env('replEnv')}"/>
  </xsl:function>
  <xsl:function name="env:close-with-binds">
    <xsl:param name="env"/>
    <xsl:param name="binds"/>
    <xsl:param name="exprs"/>
    <xsl:variable name="new-env" select="map {'outer': env:noReplEnv($env), 'replEnv': $env('replEnv'), 'data': map{}, 'isReplEnv': false()}"/>
    <xsl:sequence select="$new-env =&gt; env:bind-all($binds, $exprs)"/>
  </xsl:function>
  <xsl:function name="env:dump">
    <xsl:param name="env"/>
    <xsl:sequence select="if (not(empty($env))) then
                            map:merge(($env('data'), env:dump($env('outer'))))
                          else
                            map{}"/>
  </xsl:function>
  <xsl:function name="env:merge">
    <xsl:param name="env"/>
    <xsl:param name="second"/>
    <xsl:variable name="env-items" select="env:dump($env)"/>
    <xsl:variable name="second-items" select="env:dump($second)"/>
    <xsl:variable name="new-env" select="if (empty($env)) then
                                            $second
                                        else if (empty($second)) then
                                            $env
                                        else
                                            map {'outer': $env('outer'), 'data': map:merge(($env-items, $second-items)), 'isReplEnv': false(), 'replEnv': $env('replEnv')}"/>
    <xsl:sequence select="$new-env"/>
  </xsl:function>
  <xsl:function name="env:hier">
    <xsl:param name="env"/>
    <xsl:param name="over"/>
    <xsl:variable name="newEnv" select="env:merge($over, $env)"/>
    <xsl:sequence select="map{'outer': env:noReplEnv($newEnv), 'data': map{}, 'isReplEnv': false(), 'replEnv': $env('replEnv')}"/>
  </xsl:function>
  <xsl:function name="env:bind-all">
    <xsl:param name="env"/>
    <xsl:param name="binds"/>
    <xsl:param name="exprs"/>
    <xsl:choose>
      <xsl:when test="exists($binds) and exists($exprs)">
        <xsl:choose>
          <xsl:when test="string(head($binds)) = '&amp;'">
            <xsl:variable name="listExprs">
              <malval kind="list">
                <lvalue>
                  <xsl:sequence select="$exprs"/>
                </lvalue>
              </malval>
            </xsl:variable>
            <xsl:sequence select="$env =&gt; env:set(string($binds[2]), $listExprs)"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:variable name="new-env" select="$env =&gt; env:set(string(head($binds)), head($exprs))"/>
            <xsl:sequence select="$new-env =&gt; env:bind-all(tail($binds), tail($exprs))"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:when test="exists($binds)">
        <xsl:choose>
          <xsl:when test="string(head($binds)) = '&amp;'">
            <xsl:variable name="listExprs">
              <malval kind="list">
                <lvalue>
                                </lvalue>
              </malval>
            </xsl:variable>
            <xsl:sequence select="$env =&gt; env:set(string($binds[2]), $listExprs)"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:variable name="Exprs">
              <malval kind="nil">
                            </malval>
            </xsl:variable>
            <xsl:variable name="new-env" select="$env =&gt; env:set(string(head($binds)), $Exprs)"/>
            <xsl:sequence select="$new-env =&gt; env:bind-all(tail($binds), $exprs)"/>
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
  <xsl:function name="fn:makeMALValue">
    <xsl:param name="value"/>
    <xsl:param name="kind"/>
    <malval kind="{$kind}" value="{$value}"/>
  </xsl:function>
</xsl:stylesheet>
