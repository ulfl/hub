# bookstore us production

## ssh

### app01 application
    ssh centos@app01.production.bookshop3425987.com

### app02 application
    ssh centos@app02.production.bookshop3425987.com

### app03 application
    ssh centos@app03.production.bookshop3425987.com

### db01 database
    ssh centos@db01.production.bookshop3425987.com

### db02 database
    ssh centos@db02.production.bookshop3425987.com

## ping check system up
    curl -v https://google.com/production/bookshop3425987/api/v1/ping

## splunk dashboards
    echo "run command to open relevant dashboards"


# bookstore staging

## ssh 

### app01 application
    ssh centos@app01.staging.bookshop3425987.com

### app02 application
    ssh centos@app02.staging.bookshop3425987.com

### app03 application
    ssh centos@app03.staging.bookshop3425987.com

### db01 database
    ssh centos@db01.staging.bookshop3425987.com

### db02 database
    ssh centos@db02.staging.bookshop3425987.com

## ping check system up
    curl -v https://google.com/staging/bookshop3425987/api/v1/ping

## splunk dashboards
    echo "command to open relevant dashboards"

## performance test environement
    echo "run command to open relevant performance test dashboards"
