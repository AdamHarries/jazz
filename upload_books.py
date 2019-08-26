import boto3

s3 = boto3.client('s3')

bucket_name = 'stompin-at-summerhall-resources'
for name in ['eb', 'bb', 'c']:
    filename = 'books/'+name+".pdf"
    response = s3.upload_file(filename, bucket_name, filename)
    print(str(response))

