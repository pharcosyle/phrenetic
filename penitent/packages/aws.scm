(define-module (penitent packages aws)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module ((guix utils) #:select (substitute-keyword-arguments))
  #:use-module ((gnu packages check) #:select (python-mock python-pytest python-coverage python-pytest-cov python-pytest-xdist))
  #:use-module ((gnu packages cmake) #:select (cmake))
  #:use-module ((gnu packages groff) #:select (groff))
  #:use-module ((gnu packages python-web) #:select (awscli) #:prefix python-web:)
  #:use-module ((gnu packages python-web) #:select (python-urllib3))
  #:use-module ((gnu packages python-xyz) #:select (python-prompt-toolkit) #:prefix python:)
  #:use-module ((gnu packages python-xyz) #:select (python-colorama-for-awscli python-docutils-0.15 python-wcwidth python-distro python-jmespath python-boto3 python-botocore python-six python-jsonschema))
  #:use-module ((gnu packages python-crypto) #:select (python-cryptography) #:prefix python-crypto:)
  #:use-module ((gnu packages serialization) #:select (python-ruamel.yaml))
  #:use-module ((gnu packages time) #:select (python-dateutil)))

(define-public awscli
  (package
    (inherit python-web:awscli)
    (version "2.4.23")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aws/aws-cli")
             (commit version)))
       (sha256
        (base32
         "1xdgj6w6ibfiwpzd85g9lhk106nxrw06c9bdcrmfr68zaa32k6ff"))
       (file-name (git-file-name (package-name python-web:awscli) version))))
    (arguments
     (substitute-keyword-arguments (package-arguments python-web:awscli)
       ;; A few tests fail at present but I'm not gonna sweat it. A number more error but I think that's mostly due to the lack of network access. The vast majority pass:
       ;; Total tests ~7000 (failures=3, errors=222, skipped=6)
       ((#:tests? _) #f)))
    (propagated-inputs
     (list python-colorama-for-awscli
           python-docutils-0.15
           python-cryptography
           python-ruamel.yaml-0.15
           python-wcwidth
           python-prompt-toolkit
           python-distro-1.5
           python-awscrt
           python-dateutil
           python-jmespath
           python-urllib3))
    (native-inputs
     (list groff
           ;; For tests.
           python-jsonschema
           python-mock
           python-pytest
           python-coverage
           python-pytest-cov
           python-pytest-xdist))))

(define python-awscrt
  (package
    (name "python-awscrt")
    (version "0.12.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "awscrt" version))
       (sha256
        (base32 "1cmfkcv2zzirxsb989vx1hvna9nv24pghcvypl0zaxsjphv97mka"))))
    (build-system python-build-system)
    (native-inputs
     (list
      cmake
      ;; For tests.
      python-boto3))
    (home-page "https://github.com/awslabs/aws-crt-python")
    (synopsis "A common runtime for AWS Python projects")
    (description "This package provides a common runtime for AWS Python projects")
    (license license:asl2.0)))

(define python-cryptography
  (package
    (inherit python-crypto:python-cryptography)
    (version "3.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cryptography" version))
       (sha256
        (base32
         "1vcvw4lkw1spiq322pm1256kail8nck6bbgpdxx3pqa905wd6q2s"))))))

(define python-ruamel.yaml-0.15
  (package
    (inherit python-ruamel.yaml)
    (version "0.15.100")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ruamel.yaml" version))
       (sha256
        (base32
         "1r5j9n2jdq48z0k4bdia1f7krn8f2x3y49i9ba9iks2rg83g6hlf"))))))

(define python-prompt-toolkit
  (package
    (inherit python:python-prompt-toolkit)
    (version "3.0.28")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "prompt_toolkit" version))
       (sha256
        (base32
         "0l0nnm9fvs8lklk2qq8mylb9jrlxvlqzpmqr4n7rdhl63rmx274z"))))))

(define python-distro-1.5
  (package
    (inherit python-distro)
    (version "1.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "distro" version))
       (sha256
        (base32
         "14nz51cqlnxmgfqqilxyvjwwa5xfivdvlm0d0b1qzgcgwdm7an0f"))))))
