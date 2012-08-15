<?xml version="1.0" encoding="UTF-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text"/>



  <xsl:template match="/">

-module(tr_soap_parser).

-compile(export_all).

-include_lib("xmerl/include/xmerl.hrl").

-include("tr69.hrl").
-include("proto.hrl").

-import(tr_soap_lib, [get_local_name/2,
		      parse_error/2]).
		      
-import(tr_soap_types, [


   <xsl:for-each select="*[name()='xs:schema']/*[name()='xs:simpleType']/*[name()='xs:restriction']"> 
                      parse_<xsl:value-of select="../@name"/>/2,   </xsl:for-each>

   <xsl:for-each select=".//*[name()='xs:element']/*[name()='xs:simpleType']">
                      parse_<xsl:value-of select="../@name"/>/2,   </xsl:for-each>

   <xsl:for-each select=".//*[name()='xs:element'][@name and (@type='xs:boolean' or @type='xs:unsignedInt' or @type='xs:int' or @type='xs:string' or @type='xs:dateTime')]">
                      parse_<xsl:value-of select="@name"/>/2,   </xsl:for-each>

   <!-- 
-->

		      parse_unsignedInt/1,
		      parse_unsignedInt/2,
		      parse_string/1,
		      parse_boolean/2,
		      parse_int/1,
		      parse_int/2,
		      parse_dateTime/2,
		      parse_base64/2,
		      parse_anySimpleType/2
		      ]).


<!--
--> 
%% Complex Type Mapping
<xsl:for-each select=".//*[name()='xs:element'][@name and @type and not(@type='xs:boolean' or @type='xs:unsignedInt' or @type='xs:int' or @type='xs:string' or @type='xs:dateTime')]">
parse_<xsl:value-of select="@name"/>(E,_S) -> parse_<xsl:value-of select="@type"/>(E,_S). </xsl:for-each>


    <!-- 
    -->
	 <xsl:apply-templates/> 
%% end
  </xsl:template>

  <xsl:template match="text()"/>



<!--
%% FOLD  <xsl:value-of select="@name"/>
-->
<xsl:template match="*[name()='xs:schema']/*[name()='xs:element' and .//*[name()='xs:sequence']]">
<xsl:variable name="container" select="@name"/>
%% -spec parse_<xsl:value-of select="@name"/>(#xmlElement{},#decoder{}) -> #camel_<xsl:value-of select="@name"/>{}.
parse_<xsl:value-of select="@name"/>(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, <xsl:value-of select="@name"/>) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    <xsl:for-each select=".//*[name()='xs:sequence']/*[name()='xs:element']">
                            '<xsl:value-of select="@name"/>' ->
                                <xsl:value-of select="$container"/>#camel_<xsl:value-of select="$container"/>{camel_<xsl:value-of select="@name"/> = parse_<xsl:value-of select="@name"/>(Elem, State)};
    </xsl:for-each>
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #camel_<xsl:value-of select="@name"/>{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).
    <xsl:apply-templates/>
  </xsl:template>

<!--
%% Simple element
-->
  <xsl:template match="*[name()='xs:schema']/*[name()='xs:element' and not(.//*[name()='xs:sequence'])]">
%% -spec parse_<xsl:value-of select="@name"/>(#xmlElement{},#decoder{}) -> #camel_<xsl:value-of select="@name"/>{}.
parse_<xsl:value-of select="@name"/>(_, _) -> #camel_<xsl:value-of select="@name"/>{}.
    <xsl:apply-templates/>
  </xsl:template>




<!--
%% COMPLEX TYPE <xsl:value-of select="@name"/>
-->
  <xsl:template match="*[name()='xs:schema']/*[name()='xs:complexType']">
    <xsl:variable name="container" select="@name"/>
%% -spec parse_<xsl:value-of select="@name"/>(#xmlElement{},#decoder{}) -> #camel_<xsl:value-of select="@name"/>{}.
parse_<xsl:value-of select="@name"/>(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, <xsl:value-of select="@name"/>) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    <xsl:for-each select=".//*[name()='xs:sequence']/*[name()='xs:element']">
                            '<xsl:value-of select="@name"/>' ->
                                <xsl:value-of select="$container"/>#camel_<xsl:value-of select="$container"/>{camel_<xsl:value-of select="@name"/> = parse_<xsl:value-of select="@name"/>(Elem, State)};
    </xsl:for-each>
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #camel_<xsl:value-of select="@name"/>{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).
    <xsl:apply-templates/>
  </xsl:template>


</xsl:stylesheet>
