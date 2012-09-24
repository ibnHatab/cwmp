<?xml version="1.0" encoding="UTF-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text"/>

  <xsl:template match="/">
%% begin
-module(tr_soap_parser).

-compile(export_all).

-include_lib("xmerl/include/xmerl.hrl").

-include("tr69.hrl").
-include("proto.hrl").


%%%-----------------------------------------------------------------------------
%%%        Build RPC Message
%%%-----------------------------------------------------------------------------

%%%-----------------------------------------------------------------------------
%%%        Build SOAP Envelope
%%%-----------------------------------------------------------------------------

%%  Build header 
%%  Build body  

%%%-----------------------------------------------------------------------------
%%%        Build SOAP Fault 
%%%-----------------------------------------------------------------------------

%%%-----------------------------------------------------------------------------
%%%        Build RPC Methods
%%%-----------------------------------------------------------------------------



<!--
--> 
%% Complex Type Mapping -- maybe tag

<xsl:for-each select=".//*[name()='xs:element'][@name and @type and not(@type='xs:boolean' or @type='xs:unsignedInt' or @type='xs:int' or @type='xs:string' or @type='xs:dateTime')]">
parse_<xsl:value-of select="@name"/>(E,_S) -> parse_<xsl:value-of select="@type"/>(E,_S). </xsl:for-each>

    <!-- 
    -->

    <xsl:apply-templates/> 
%% end
  </xsl:template>
  
  <xsl:template match="text()"/>




  <!-- 
       Build RPC Methods
       Complex element
  -->
<xsl:template match="*[name()='xs:schema']/*[name()='xs:element' and .//*[name()='xs:sequence']]">
<xsl:variable name="container" select="@name"/>
-spec build_<xsl:value-of select="$container"/>(#%camel%<xsl:value-of select="$container"/>{}, #builder{}) -> export_element().
build_<xsl:value-of select="$container"/>(Data, State) ->   
    {'cwmp:<xsl:value-of select="$container"/>', [],
     [P || P - [
    <xsl:for-each select=".//*[name()='xs:sequence']/*[name()='xs:element']"> build_<xsl:value-of select="@name"/>(Data#%camel%<xsl:value-of select="$container"/>.%camel%<xsl:value-of select="@name"/>, State),
    </xsl:for-each> ], P /= null]}.
    <!-- 
    -->
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
