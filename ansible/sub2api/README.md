# Sub2API Ansible Deployment

Deploys [Sub2API](https://github.com/Wei-Shaw/sub2api) AI API gateway via Docker Compose.

## Usage

1. Edit `inventory.ini` with your server IP
2. Optionally override vars in `vars/main.yml`
3. Run:

```bash
ansible-playbook -i inventory.ini deploy-sub2api.yml
```

## Post-Deployment

Access `http://<server_ip>:8080` and complete the setup wizard.
