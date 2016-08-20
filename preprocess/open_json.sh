for y in *.json.gz;
do
    echo Converting $y
    echo '------------'
    gzip -d -k $y
    python convert_json_bash.py *.json
    rm *.json
    echo '------------'
done