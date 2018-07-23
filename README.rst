=========
terradiff
=========

.. image:: https://circleci.com/gh/jml/terradiff/tree/master.svg?style=svg
    :target: https://circleci.com/gh/jml/terradiff/tree/master
    :alt: CircleCI build of master

.. image:: https://quay.io/repository/jml0/terradiff/status
    :target: https://quay.io/repository/jml0/terradiff
    :alt: Docker Repository on Quay

Get told when reality no longer matches your Terraform configuration.

This project is not ready for general use.
This README is an infelicitous mix of documentation, plans, aspirations, and notes to self.

Why you might want this
=======================

Say you've got some `Terraform`_ configuration in a Git repository somewhere.

You will have some way of applying this configuration to your environments.
You might run ``terraform`` manually, you might `run it from CI`_, or you
might use a tool like `Atlantis`_.

.. _`run it from CI`: https://www.terraform.io/guides/running-terraform-in-automation.html
.. _`Atlantis`: https://www.runatlantis.io/

No matter which of these you do, there might still be times when your actual
environment will differ from what you intend in your configuration. Perhaps
the config fails to apply. Perhaps someone made a direct change to the
environment, circumventing your Terraform.

When this happens, you want to be told. In fact, you want to be alerted, so
you can take whatever action is necessary to reconcile your configuration and
reality.

What this does
==============

When deployed, terradiff monitors a Terraform configuration and runs
``terraform plan`` every so often (every 2 minutes, say). It exports a
Prometheus `gauge`_, ``terradiff_plan_exit_code``, that indicates whether
``terraform plan`` succeeded with no diff (0), failed due to some kind of
error (1), or succeeded with some kind of diff (2). See the `terraform plan
manual`_ for more details.

You can then configure a `Prometheus alert`_ that will tell you when there's a
diff, or when the diffing process is broken.

terradiff also serves a simple web UI that shows the full ``terraform plan``
output. Your alert should link to that page so you can figure out what to do.

How to deploy it
================

terradiff is designed to run on Kubernetes. It is cloud native, if you're into
that sort of thing.

It expects to run with a `git-sync`_ `sidecar`_ that pulls in your Terraform
configuration from Git.

An `example Kubernetes Deployment manifest <doc/terradiff-dep.yaml>`_ can be
found in this repository. It assumes you have a `Secret`_ named
``git-sync-secret`` with your GitHub credentials for synchronising the
repository with your Terraform configuration, and Secrets for any credentials
required to run ``terraform plan`` on that configuration.

`Example alerting rules <doc/terradiff.rules.yaml>`_ are also provided.

History
=======

This project is inspired by the use of Terraform at `Weaveworks`_. In
particular, its lineage includes `prom-run`_.

How to build this project
=========================

You really want to have `stack`_ installed, and to invoke it directly.

.. _`Prometheus alert`: https://prometheus.io/docs/prometheus/latest/configuration/alerting_rules/
.. _`Secret`: https://kubernetes.io/docs/concepts/configuration/secret/
.. _`Terraform`: https://terraform.io
.. _`Weaveworks`: https://weave.works
.. _`gauge`: https://prometheus.io/docs/concepts/metric_types/#gauge
.. _`git-sync`: https://github.com/kubernetes/git-sync
.. _`prom-run`: https://github.com/tomwilkie/prom-run
.. _`sidecar`: https://kubernetes.io/blog/2015/06/the-distributed-system-toolkit-patterns/
.. _`stack`: https://docs.haskellstack.org/en/stable/README/
.. _`terraform plan manual`: https://www.terraform.io/docs/commands/plan.html#detailed-exitcode
