# Build

`nix-build`

## Setup

### Secrets

To test the log-processor, you need public/private prime and exponent values. You can generate an RSA key and get the values using the following commands:

`$ openssl genrsa -out key.pem`

`$ openssl rsa -inform PEM -text -noout -in key.pem`

### Environment Variables

The following environment variables are supported:

| Name                       | Type      | Required | Description                                                                                     |
|----------------------------|-----------|----------|-------------------------------------------------------------------------------------------------|
| SYMMETRIC_ENCRYPTION_KEY   | Text      | Yes      | Base64 encoded symmetric encryption key                                                         |
| OUT_PAYLOAD_ENCRYPTION_KEY | Text      | No       | Base64 encoded symmetric encryption key used to encrypt logs going out of the log-processor     |
| HTTP_PORT                  | Int       | Yes      | Port the log processor will run on                                                              |
| PUBLIC_SIZE                | Int       | Yes      | Key size in bytes. The size you have is mostly in bits, convert it to bytes.                    |
| PRIVATE_P                  | Int       | Yes      | `openssl` output section: prime1 (in decimal)                                                   |
| PRIVATE_Q                  | Int       | Yes      | `openssl` output section: prime2 (in decimal)                                                   |
| PUBLIC_N                   | Int       | Yes      | `openssl` output section: modulus (in decimal)                                                  |
| PUBLIC_E                   | Int       | Yes      | `openssl` output section: publicExponent                                                        |
| PRIVATE_EXPONENT           | Int       | Yes      | `openssl` output section: privateExponent (in decimal)                                          |
| METRICS_PORT               | Int       | Yes      | Port the Prometheus server will run on                                                          |
| KAFKA_BROKERS              | List Text | Yes      | List of Kafka broker addresses                                                                  |
| JSON_LOGS_TOPIC            | Text      | Yes      | Kafka topic used for sending JSON encoding logs (topic should already exist)                    |
| MAX_IN_MEMORY_MESSAGES     | Int       | Yes      | Number of messages stored in memory                                                             |

You can refer to `.envrc` for a list of default options.

# Run
`nix-build`/bin/log-processor -c ./config.dhall
# encode-decode
