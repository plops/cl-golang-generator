curl \
    http://localhost:8080/albums \
    --include \
    --header "Content-Type: application/json" \
    --request "POST" \
    --data '{"id":"4","title":"The Modern Sound of Betty Carter","artist":"Betty Carter","price":49.99}'