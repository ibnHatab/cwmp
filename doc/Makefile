

all: xslt

xql:
	java -classpath ~/libs/saxon9he.jar net.sf.saxon.Query -q script.xql

xslt: tr_soap_parser.xslt
	xsltproc tr_soap_parser.xslt cwmp-1-2.xsd | sed -e 's/_xs:/_/' -e 's/_tns:/_/' -e 's/_soapenc:/_/'\
	| tee tr_soap_parser.erl
