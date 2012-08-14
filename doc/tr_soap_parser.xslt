<?xml version="1.0" encoding="UTF-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text"/>



  <xsl:template match="/">
    <xsl:text>
-module(tr_soap_parser).

-compile(export_all).

-include_lib("xmerl/include/xmerl.hrl").

-include("tr69.hrl").
-include("proto.hrl").

-import(tr_soap_lib, [get_local_name/2,
		      parse_error/2,
		      
		      parse_anySimpleType/2,
		      xmlElement/1,
		      parse_unsignedInt/1,
		      parse_unsignedInt/2,
		      parse_string/1,
		      parse_boolean/2,
		      parse_int/1,
		      parse_int/2,
		      parse_dateTime/2,
		      parse_base64/2,

		      parse_SetParameterValuesFault/2,
		      parse_ParameterName/2,
		      parse_ParameterList/2,
		      parse_ParameterKey/2,
		      parse_Status/2,
		      parse_ParameterPath/2,
		      parse_NextLevel/2,
		      parse_ObjectName/2,
		      parse_InstanceNumber/2,
		      
		      parse_CommandKey/2,
		      parse_FileType/2,
		      parse_URL/2,
		      parse_Username/2,
		      parse_Password/2,
		      parse_FileSize/2,
		      parse_TargetFileName/2,
		      parse_DelaySeconds/2,
		      parse_SuccessURL/2,
		      parse_FailureURL/2,
		      parse_StartTime/2,
		      parse_CompleteTime/2,
		      parse_OptionName/2,
		      parse_Operations/2,
		      parse_CommandKey/2,
		      parse_DeviceId/2,
		      parse_Event/2,
		      parse_MaxEnvelopes/2,
		      parse_CurrentTime/2,
		      parse_RetryCount/2,
		      parse_FaultStruct/2,
		      parse_AnnounceURL/2,
		      parse_TransferURL/2,
		      parse_IsDownload/2,
		      parse_Command/2,
		      parse_Referer/2,
		      parse_Arg/2,
		      parse_Next/2,
		      parse_NextURL/2,
		      parse_Results/2

]).


    </xsl:text>

parse_FaultCode(E, _) -> parse_unsignedInt(E).
parse_FaultString(E, _) -> parse_string(E).

parse_string(E, _) -> parse_string(E).
parse_Name(E, _) -> parse_string(E).
parse_Value(E, S) -> parse_anySimpleType(E, S).

parse_Manufacturer(E, _) -> parse_string(E).
parse_OUI(E, _) -> parse_string(E).
parse_ProductClass(E, _) -> parse_string(E).
parse_SerialNumber(E, _) -> parse_string(E).

    <xsl:apply-templates/>

    <xsl:text>
%% end
    </xsl:text>
  </xsl:template>

  <xsl:template match="text()"/>

  <xsl:template match="*[name()='xs:schema']/*[name()='xs:element' and .//*[name()='xs:sequence']]">
<!--
%% FOLD  <xsl:value-of select="@name"/>
-->
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

  <xsl:template match="*[name()='xs:schema']/*[name()='xs:element' and not(.//*[name()='xs:sequence'])]">
<!--
%% Simple element
-->
%% -spec parse_<xsl:value-of select="@name"/>(#xmlElement{},#decoder{}) -> #camel_<xsl:value-of select="@name"/>{}.
parse_<xsl:value-of select="@name"/>(_, _) -> #camel_<xsl:value-of select="@name"/>{}.
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="*[name()='xs:schema']/*[name()='xs:simpleType']">
<!--
-->
%% Simple type
parse_<xsl:value-of select="@name"/>(E,_S) -> parse_<xsl:value-of select="*[name()='xs:restriction']/@base"/>(E).
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="*[name()='xs:schema']/*[name()='xs:complexType']">
    <xsl:variable name="container" select="@name"/>
<!--
%% COMPLEX TYPE <xsl:value-of select="@name"/>
-->
%% -spec parse_<xsl:value-of select="@name"/>(#xmlElement{},#decoder{}) -> #camel_<xsl:value-of select="@name"/>{}.
parse_<xsl:value-of select="@name"/>(#xmlElement{content = Content} = _Elems, #decoder{ns=Nss} = State) ->
    lists:foldl(fun(Elem, <xsl:value-of select="@name"/>) ->
                        case get_local_name(Elem#xmlElement.name, Nss#rpc_ns.ns_cwmp) of
    <xsl:for-each select=".//*[name()='xs:sequence']/*[name()='xs:element']">
                            '<xsl:value-of select="@name"/>' ->
                                <xsl:value-of select="$container"/>#camel_<xsl:value-of select="$container"/>{camel_<xsl:value-of select="@name"/> = parse_<xsl:value-of select="@type"/>(Elem, State)};
    </xsl:for-each>
                            _ ->
                                parse_error(Elem, State)
                        end
                end,
                #camel_<xsl:value-of select="@name"/>{}, lists:filter(fun tr_soap_lib:xmlElement/1, Content)).
<!--
-->
    <xsl:apply-templates/>
  </xsl:template>


</xsl:stylesheet>
