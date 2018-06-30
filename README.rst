===========
mass-driver
===========

.. image:: https://circleci.com/gh/jml/mass-driver/tree/master.svg?style=svg
    :target: https://circleci.com/gh/jml/mass-driver/tree/master

.. what is mass-driver? why would I want to use it? how do I use it?

.. After instantiating this template, you need to do a few things manually:
   1. Add a synopsis to ``package.yaml``. It should be a short, one sentence description of your project.
   2. Rename ``src/Lib.hs`` to the module name of your package. Typically this is your package name in ``CamelCase``.
   3. Update ``cmd/Main.hs`` and ``tests/Tasty.hs`` to refer to the new package.
   4. Add the following to ``stack.yaml``:
       image:
         container:
           base: quay.io/jml0/mass-driver-base
           name: quay.io/jml0/mass-driver
           executables:
             - mass-driver
   5. Write a decent README
   6. Update the LTS image version in ``.circleci/config.yml``
   7. Register the repository on CircleCI
   8. If the project is private, issue a token and update the shield URL above (see https://circleci.com/docs/2.0/status-badges/)
   9. Update the CircleCI configuration to log in to *your* image registry, if you are not using quay.io
   10. Install hlint: ``stack install hlint``
   11. Delete these comments

How to build this project
=========================

You really want to have `stack`_ installed, and to invoke it directly.

.. _`stack`: https://docs.haskellstack.org/en/stable/README/
