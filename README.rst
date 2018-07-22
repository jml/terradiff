=================================
mass-driver: Terraform with style
=================================

.. image:: https://circleci.com/gh/jml/mass-driver/tree/master.svg?style=svg
    :target: https://circleci.com/gh/jml/mass-driver/tree/master

tl;dr
=====

 - Generate test Terraform plans for GitHub PRs
 - Manual review for Terraform plans generated from approved PRs about to be applied
 - Apply Terraform plans

This project is not ready for general use.
This README is an infelicitous mix of documentation, plans, aspirations, and notes to self.

Example workflow
================

Creating a GitHub repo
----------------------

 1. Alex wants to create a ``mariner`` repo that anyone in the ``roci`` team can read and write to
 2. Alex submits a PR to ``corp`` with Terraform configuration for such access
 3. Bobbi sees the PR, but notes that there's no plan generated for it yet. She makes a note to come back in a while.
 4. Bobbi comes back and sees the GitHub check that says a plan was successfully generated. The plan has also been posted as a comment by ``mass-driver`` bot
 5. Bobbi thinks both the PR and plan look good, so she approves the PR
 6. ``mass-driver`` detects the approval and adds this plan/PR to its queue
 7.  When the plan/PR reaches the front of the queue, ``mass-driver`` generates an authoritative plan that, if approved, will be applied to the environment
 8. ``mass-driver`` updates the PR, stating that the plan is ready for final approval before being applied, and giving a link to the apply page
 9. Bobbi and Alex are notified through normal GitHub means that the plan for the PR is ready to be applied
 10. Bobbi reviews the final plan, and indicates that it is OK to be applied now
 11. ``mass-driver`` applies the plan.
 12. ``mass-driver`` adds a comment to the PR confirming application and closes the PR

Reflections on workflow
-----------------------

Item 3 is obviously suboptimal.
Ideally, ``mass-driver`` would request review only after generating a plan that can be reviewed.

We have a series of open questions about how to mark approval. There are two approval steps:

  1. PR is OK to be enqueued
  2. Final plan from PR is OK to be applied

The questions are:

  1. Are both of these things necessary?
  2. How do we mark approval in each case?

     - what is the UI?
     - who is authorized to approve, and how?
     - where do we store the approval status?

Broadly, the options for 2 are either:

  1. Comments on GitHub
  2. Buttons on the ``mass-driver`` UI


.. _`Terraform`: https://terraform.io

How to build this project
=========================

You really want to have `stack`_ installed, and to invoke it directly.

.. _`stack`: https://docs.haskellstack.org/en/stable/README/
