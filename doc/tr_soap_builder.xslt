<?xml version="1.0" encoding="UTF-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text"/>

  <xsl:template match="/">
<!--
%% begin
-module(tr_soap_parser).

-compile(export_all).

-include_lib("xmerl/include/xmerl.hrl").

-include("tr69.hrl").
-include("proto.hrl").

--> 

    <xsl:apply-templates/> 
%% end
  </xsl:template>
  
  <xsl:template match="text()"/>


  <!-- 
       OK Build Parameter List
       Array complexType/complexContent/restriction base="soapenc:Array" 
       Generate list of sequence/element
<xsl:template match="*[name()='xs:schema']/*[name()='xs:complexType' and .//*[name()='xs:complexContent' and ./*[name()='xs:restriction']/@base='soapenc:Array']]">
<xsl:variable name="containerList" select="@name"/>
<xsl:variable name="elementStruct" select=".//*[name()='xs:element']/@name"/>
%-spec build_<xsl:value-of select="$containerList"/>([#%camel%<xsl:value-of select="$elementStruct"/>{}], #builder{}) -> export_element().
build_<xsl:value-of select="$containerList"/>(Data,  _State) ->
    TagArray =  [build_<xsl:value-of select="$elementStruct"/>(E) || E - Data],
    {'cwmp:<xsl:value-of select="$containerList"/>', [attr_arrayType(length(TagArray))], TagArray}.
    <xsl:apply-templates/>
</xsl:template>

  -->

  
  <!-- 
       Build RPC Methods
 
      Complex element
  -->
    <!-- OK schema/element/sequence
<xsl:template match="*[name()='xs:schema']/*[name()='xs:element' and .//*[name()='xs:sequence']]">

<xsl:variable name="container" select="@name"/>
-spec build_<xsl:value-of select="$container"/>(#%camel%<xsl:value-of select="$container"/>{}, #builder{}) -> export_element().
build_<xsl:value-of select="$container"/>(Data, State) ->   
    {'cwmp:<xsl:value-of select="$container"/>', [],
     [P || P - [
    <xsl:for-each select=".//*[name()='xs:sequence']/*[name()='xs:element']"> build_<xsl:value-of select="@name"/>(Data#%camel%<xsl:value-of select="$container"/>.%camel%<xsl:value-of select="@name"/>, State),
    </xsl:for-each> ], P /= null]}.
    <xsl:apply-templates/>
</xsl:template>
 
    -->

    <!-- OK schema/element/sequence
    -->
<xsl:template match="*[name()='xs:schema']/*[name()='xs:complexType' and .//*[name()='xs:sequence']]">

<xsl:variable name="container" select="@name"/>
-spec build_<xsl:value-of select="$container"/>(#%camel%<xsl:value-of select="$container"/>{}, #builder{}) -> export_element().
build_<xsl:value-of select="$container"/>(Data, State) ->   
    {'cwmp:<xsl:value-of select="$container"/>', [],
     [P || P - [
    <xsl:for-each select=".//*[name()='xs:sequence']/*[name()='xs:element']"> build_<xsl:value-of select="@name"/>(Data#%camel%<xsl:value-of select="$container"/>.%camel%<xsl:value-of select="@name"/>, State),
    </xsl:for-each> ], P /= null]}.
    <xsl:apply-templates/>
</xsl:template>
 


<!--
    Build RPC Methods
    Simple element
-->
  <!--
  <xsl:template match="*[name()='xs:schema']/*[name()='xs:element' and not(.//*[name()='xs:sequence'])]">
<xsl:variable name="container" select="@name"/>%% -spec build_<xsl:value-of select="@name"/>(#camel_<xsl:value-of select="@name"/>{}) ->  #xmlElement{}.
    <xsl:apply-templates/>
  </xsl:template>

      RPC Methods Arguments
      Complex Type Definitions
  -->

  <!-- 
       RPC Methods Arguments
       Simple Type Definitions
  -->

</xsl:stylesheet>
