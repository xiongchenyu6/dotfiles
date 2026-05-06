# Casdoor Ansible Deployment

Deploys [Casdoor](https://github.com/casdoor/casdoor) (UI-first IAM / SSO) via
Docker Compose with a PostgreSQL backend and Redis session store.

## Usage

```bash
cd ansible/casdoor
ansible-playbook -i inventory.ini deploy-casdoor.yml
```

Target host is hard-coded in `inventory.ini` to `root@203.116.95.146`.
Override deploy variables in `vars/main.yml`.

## Post-Deployment

Browse to `http://203.116.95.146:8000` and sign in with the default
`admin / 123` account, then change the password immediately and update
`casdoor_origin` in `vars/main.yml` once a TLS reverse proxy is in place.

## Layout

| File                              | Purpose                                  |
|-----------------------------------|------------------------------------------|
| `deploy-casdoor.yml`              | Main playbook                            |
| `inventory.ini`                   | Target host                              |
| `vars/main.yml`                   | Tunables (ports, image, origin, TZ)      |
| `templates/app.conf.j2`           | Casdoor server config (`conf/app.conf`)  |
| `templates/docker-compose.yml.j2` | Compose stack: casdoor + postgres + redis|
