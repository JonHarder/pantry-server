terraform {
  backend "gcs" {
    bucket = "pantry-app"
    prefix = "terraform"
  }
}

provider "google" {
  version = "3.5.0"

  project = "pantry-268521"
  region  = "us-central1"
  zone    = "us-central1-c"
}

resource "google_container_cluster" "primary" {
  name = "pantry-cluster"
  remove_default_node_pool = true
  initial_node_count = 1

  master_auth {
    username = ""
    password = ""

    client_certificate_config {
      issue_client_certificate = false
    }
  }
}

resource "google_container_node_pool" "primary_preemptible_nodes" {
  name       = "pantry-node-pool"
  cluster    = google_container_cluster.primary.name
  node_count = 1

  management {
      auto_repair = "true"
      auto_upgrade = "true"
  }

  node_config {
    preemptible  = true
    machine_type = "g1-small"

    metadata = {
      disable-legacy-endpoints = "true"
    }

    oauth_scopes = [
      "https://www.googleapis.com/auth/logging.write",
      "https://www.googleapis.com/auth/monitoring",
    ]
  }
}