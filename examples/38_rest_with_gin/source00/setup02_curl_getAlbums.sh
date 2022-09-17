set -x
curl \
    http://localhost:8080/albums \
    --header "Content-Type: application/json" \
    --request "GET"
