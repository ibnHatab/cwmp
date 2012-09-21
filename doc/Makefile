

all: xslt

xql:
	java -classpath ~/libs/saxon9he.jar net.sf.saxon.Query -q script.xql

%.erl: %.xslt
	xsltproc $< cwmp-1-2.xsd \
	| sed -e 's/_xs:/_/' -e 's/_tns:/_/' -e 's/_soapenc:/_/'\
	| tee $@


.PHONY: xslt
xslt: cwmp-1-2.xsd tr_soap_parser.erl tr_soap_types.erl tr_soap_builder.erl
