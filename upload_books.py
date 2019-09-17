#!/usr/bin/env python3

import boto3

s3 = boto3.client('s3')

bucket_name = 'stompin-at-summerhall-resources'
print("Uploading " + "resources/index.html" + " to " + bucket_name + "/" + "index.html")
s3.upload_file("resources/index.html", bucket_name, "index.html", ExtraArgs={'ContentType': "text/html", 'ACL':"public-read"})
for name in ['eb', 'bb', 'c']:
    filename = 'books/' + name + ".pdf"
    print("Uploading " + filename + " to " + bucket_name + "/" + filename)
    s3.upload_file(filename, bucket_name, filename, ExtraArgs={'ContentType':"application/pdf", 'ACL':"public-read"})
