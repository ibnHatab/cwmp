

<root>
  {for $i in doc("cwmp-1-2.xsd")/xs:schema/xs:element
  return
<attribute> "rem"
%% @doc {data($i/xs:annotation/xs:documentation)} %%
  -record ({data($i/@name)} , open
  {for $j in $i//xs:element
  return
  <name> "rem"
    {data($j/@name)} :: {data($j/@type)}(),
"rem"  </name>
  }
close).

"rem" </attribute>
}
</root>