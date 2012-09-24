<?xml version="1.0" encoding="UTF-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text"/>



  <xsl:template match="/">
-module(tr_soap_types).

-compile(export_all).

-include_lib("xmerl/include/xmerl.hrl").

-include("tr69.hrl").
-include("proto.hrl").

-import(tr_soap_lib, [get_local_name/2,
		      parse_error/2
		      ]).

parse_boolean(_E) -> ok.
parse_dateTime(_E) -> ok.
parse_int(_E) -> ok.
parse_string(_E) -> ok.
parse_unsignedInt(_E) -> ok.
parse_anyURI(_E) -> ok.
    
parse_(_E) ->  ok.

parse_URL(E,_S) when is_list(E) -> parse_string(E);
parse_URL(E,_S) -> parse_anyURI(E).

    <xsl:apply-templates/>
%% end
  </xsl:template>

  <xsl:template match="text()"/>

<!--
  <xsl:template match="*[name()='xs:schema']/*[name()='xs:simpleType']/*[name()='xs:restriction']">
parse_<xsl:value-of select="../@name"/>(E,_S) -> parse_<xsl:value-of select="@base"/>(E).<xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="*[name()='xs:element']/*[name()='xs:simpleType']">
parse_<xsl:value-of select="../@name"/>(E,_S) -> parse_<xsl:value-of select="*[name()='xs:restriction']/@base"/>(E).<xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="*[name()='xs:element'][@name and (@type='xs:boolean' or @type='xs:unsignedInt' or @type='xs:int' or @type='xs:string' or @type='xs:dateTime')]">
parse_<xsl:value-of select="@name"/>(E,_S) -> parse_<xsl:value-of select="@type"/>(E).<xsl:apply-templates/>
  </xsl:template>

-->

<xsl:template match="*[name()='xs:element'][@name and (@type='xs:boolean' or @type='xs:unsignedInt' or @type='xs:int' or @type='xs:string' or @type='xs:dateTime')]">build_<xsl:value-of select="@name"/>(Data) -> maybe_tag('<xsl:value-of select="@name"/>', fun format_<xsl:value-of select="@type"/>/1, Data).
</xsl:template>


</xsl:stylesheet>
