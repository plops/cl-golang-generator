mkdir xsd
cd xsd
#curl http://schemas.opengis.net/kml/2.2.0/ogckml22.xsd -O
curl http://schemas.opengis.net/kml/2.2.0/atom-author-link.xsd -O
curl http://docs.oasis-open.org/election/external/xAL.xsd -O
curl https://developers.google.com/kml/schema/kml21.xsd -O
cd ..
~/go/bin/xgen -i xsd -o schema -l Go
