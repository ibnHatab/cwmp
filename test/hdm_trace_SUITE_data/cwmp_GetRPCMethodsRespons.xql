xquery version "1.0";
declare namespace functx = "http://www.functx.com";
declare function functx:path-to-node($nodes as node()*) as xs:string* {
    $nodes/string-join(ancestor-or-self::*/name(.), '/')
};
 
declare function functx:distinct-element-paths($nodes as node()*) as xs:string* {
    distinct-values(functx:path-to-node($nodes/descendant-or-self::*))
 };
 
declare function functx:sort($seq as item()*) as item()* {
  for $item in $seq
  order by $item
  return $item
};

declare function functx:value-except 
  ( $arg1 as xs:anyAtomicType* ,
    $arg2 as xs:anyAtomicType* )  as xs:anyAtomicType* {
       
  distinct-values($arg1[not(.=$arg2)])
 } ;
 
let $orig-xml := document('cwmp_GetRPCMethodsRespons.xml')
let $gen-xml := document('/tmp/cwmp.2012-10-12_12.35.35/cwmp_GetRPCMethodsRespons.xml')
 
let $orig-seq := functx:sort(functx:distinct-element-paths($orig-xml))
let $gen-seq := functx:sort(functx:distinct-element-paths($gen-xml))
let $result := functx:value-except($orig-seq, $gen-seq)
return <result result="{$result}"/>

