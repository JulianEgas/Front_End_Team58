# Backend

![image info](./assets/Home_ss.PNG)

[Triibu](www.rionegrodatascience.com "Frontend") is powered by _Digital Ocean_ and _Azure_ services, of which we use the following:

- Frontend deployment:
  - Digital Ocean Droplet with Ubuntu 20.04 distribution
  - 2GB RAM/ 25 GB Disk
  - 2vCPUs

- Azure Database for PostgreSQL

The full stack can be diagramed as this:

![image info](./assets/Architecture.png)

## Folder Structure

```
project_root/
│
├── assets/               # Project assets
├── database/             # Tables, dimensions and views DDL scripts
```
## Data science workflow

![image info](./assets/DS_Workflow.png)

## Database

DDL script for the database scheme creation, tables and materialized views can be found in the [database](../-/tree/master/database) folder. For this project we use _Azure Database for PostgreSQL_. Capacity of the DB instance correspond to a **db.t2.micro** instance (1vCPU, 1 Gb of RAM). 

### Er diagram

![image info](./assets/ER_diagram.png)
