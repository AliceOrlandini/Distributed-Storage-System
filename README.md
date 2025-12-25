# Distributed Storage System

Distributed, chunk-based storage system with replication, load-aware placement, and a GUI client.

Project developed for **Distributed Systems and Middleware Technologies** (University of Pisa, A.Y. 2024/2025).

## Overview

This system stores files by **splitting them into fixed-size chunks** and distributing those chunks across multiple storage nodes (“slaves”). Each chunk is **replicated** on a secondary slave to improve fault tolerance. A set of “master” nodes manages metadata, authentication, access control, and orchestrates chunk placement and replication.

Repository structure includes:
- `client/` (Java GUI client)
- `server/` (Erlang masters & slaves)
- `nginx.conf` (load balancer config)
- `Documentation.pdf` (full technical documentation)

## Architecture

**Components**
- **Client (Java GUI):** upload, download, share, delete files. 
- **Load Balancer (NGINX):** routes client requests across master nodes; uses sticky sessions via `ip_hash`.
- **Master Nodes (Erlang):** file chunking, metadata management, user auth (JWT), ACLs, chunk placement & replication coordination; metadata stored in a replicated **Mnesia** DB.
- **Slave Nodes (Erlang):** store/replicate chunks, serve chunk downloads, track local load metrics.

**Communication**
- Client ↔ backend: **HTTP**
- Master ↔ slave: **Erlang IPC**

## Supported operations

### Upload
1. Client uploads a file (JWT-authenticated).
2. Master splits the file into chunks.
3. Master assigns chunks to slaves based on current load.
4. Primary slave stores the chunk and replicates it to a secondary slave.

### Download
1. Client requests a file (JWT-authenticated).
2. Master checks permissions and selects, for each chunk, the best slave by querying candidate slaves’ load.
3. Load estimate:
   - `TotalRequests = PendingRequests + 0.5 × PossibleRequests`
4. Master returns (per chunk) the chosen slave IP + a **short-lived token**.
5. Client downloads chunks directly from slaves and reassembles the file.

### Sharing
- Master updates metadata to grant the recipient full access; file appears in recipient’s list.

### Deletion
- If requester is the last owner: master instructs all relevant slaves to delete chunks and removes metadata.
- If still shared: only requester’s access metadata is removed.

## Key features

- Chunk-based storage and distribution
- Chunk replication for fault tolerance
- Load-aware chunk placement and load-aware download routing
- Horizontal scaling of masters behind NGINX
- JWT-based authentication + per-file access control (sharing)
- Metadata replication via Mnesia
