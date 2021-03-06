.. Copyright (c) 2020 The DAML Authors. All rights reserved.
.. SPDX-License-Identifier: Apache-2.0

Migrating DAML ledgers
######################

Building migration projects
---------------------------

When we want to replace a DAML package ``foo-1.0.0`` that is deployed on a DAML ledger with an
updated version, say ``foo-2.0.0``, we need to migrate all the existing contracts that are active
and whose templates are defined in either ``foo-1.0.0`` or whose template definition depends directly
or indirectly on data defined in ``foo-1.0.0``. To help a DAML deployment with this task, the DAML
assistant offers the command ``daml migrate``.

The ``migrate`` command takes as inputs the package ``foo-1.0.0`` and its new version ``foo-2.0.0``
and creates a new project containing generated code to migrate contracts from ``foo-1.0.0`` to
``foo-2.0.0``. Run ``daml migrate --help`` to see its full usage description:

.. code-block:: none

  Usage: daml migrate TARGET_PATH FROM_PATH TO_PATH

  Available options:
    TARGET_PATH              Path where the new project should be located
    FROM_PATH                Path to the dar-package from which to migrate from
    TO_PATH                  Path to the dar-package to which to migrate to
    -h,--help                Show this help text

For example, to create a migration project from ``foo-1.0.0`` to ``foo-2.0.0`` run

.. code-block:: none

  daml migrate foo-upgrade-2.0.0 foo-1.0.0/.daml/dist/foo-1.0.0.dar foo-2.0.0/.daml/dist/foo-2.0.0.dar

This generates a migration project in the directory ``foo-upgrade-2.0.0``. To build it, change
directory to ``foo-upgrade-2.0.0`` and run

.. code-block:: none

  daml build

Writing upgrade/rollback contracts
----------------------------------

It is important to understand that the ``daml migrate`` command will not always succeed in
generating a migration project that will compile. To understand why, let's assume that the
``foo-1.0.0`` consists of a single module:

.. code-block:: none

  foo-1.0.0
  ├── daml
  │   └── Foo.daml
  ├── daml.yaml
  └── ui-backend.conf

where the ``Foo.daml`` file contains

.. literalinclude:: foo-1.0.0/daml/Foo.daml
  :language: daml
  :lines: 6-14

The package ``foo-2.0.0`` contains exactly the same modules, but a new template ``Bar`` has been
added to the ``Foo`` module.

.. literalinclude:: foo-2.0.0/daml/Foo.daml
  :language: daml
  :lines: 6-22

If we generate a migration project with ``daml migrate`` as above, the directory contents of the
``foo-2.0.0-upgrade/daml`` directory is

.. code-block:: none

  daml
  └── Foo.daml

For every template that was defined in the module ``Foo`` in the ``foo-1.0.0`` package, you will
find two new templates. One to upgrade contract instances of this template to the changed template
defined in the module ``Foo`` in the package ``foo-2.0.0`` and one to rollback the process. Here is
an example:

.. literalinclude:: foo-upgrade-2.0.0/daml/Foo.daml
  :language: daml
  :lines: 5-16

The new template types are defined via a type alias and use generic templates to update or rollback
contract instances defined in the DAML standard library (see `DA.Upgrade
<https://docs.daml.com/daml/reference/base.html#module-da-upgrade>`__). The ``Upgrade`` template
offers a choice to input a contract instance defined in ``foo-1.0.0`` and create a new one that has
been converted to a contract instance defined in ``foo-2.0.0``. The ``Rollback`` template implements
the reversed workflow.

The last line generates a ``Convertible`` instance for the ``Foo`` template of both packages. This
is a manifestation that instances of the ``Foo`` template in ``foo-1.0.0`` can be converted to
instances of the equally named template in ``foo-2.0.0``.

The ``migrate`` command only generates a suggestion for an upgrade/rollback contract of your data
types.  If your upgrade workflow doesn't match the ``Upgrade`` template provided by the standart
library you can write your own specification on how contracts from the ``foo-1.0.0`` package are
converted to contracts of the ``foo-2.0.0`` package.

The suggested ``Convertible`` instances use the shallow copy mechanism ``A.Foo{..} = B.Foo{..}``.
This will only work for trivial conversions, where the two data types ``A.Foo`` and ``B.Foo`` are
defined the same in both packages. If they are not isomorphic, the compilation will fail and you
will have to specify how to convert the data types manually. One possibility is to use ``DA.Generics``
to avoid boilerplate code. This is described in the section :ref:`using_generics_for_deep_copies`.

Deploying the migration
-----------------------

Once you've succeeded building the ``foo-upgrade-2.0.0`` package you can deploy it on the ledger
together with the ``foo-2.0.0`` package. Optionally you can bundle it with the ``foo-2.0.0`` package
into a single DAML archive by running

.. code-block:: none

  daml damlc merge-dars foo-2.0.0/.daml/dist/foo-2.0.0.dar foo-upgrade-2.0.0/.daml/dist/foo-upgrade-2.0.0.dar --package-name foo-2.0.0-with-upgrades.dar

You find more information on how to deploy DAML archive packages :ref:`here <deploy-ref_index>` .
After the ``foo-upgrade-2.0.0`` package has been deployed on the ledger, there exists for every
contract defined in ``foo-1.0.0`` a DAML workflow with a choice to upgrade it to ``foo-2.0.0`` or
roll it back.

.. _using_generics_for_deep_copies:

Using Generics for deep copies
------------------------------

When writing ``Convertible`` instances, you will often want to deep copy data types that are
*esstentially* the same. To avoid boilerplate code, you can use the default methods of the
``Convertible`` class defined via generic representations of the data types.

Generic representations can be converted when they are *isomorphic*. That means the corresponding
data types defined in package ``foo-1.0.0`` and ``foo-2.0.0`` have exactly the same shape. For
example the following two data types are isomorphic:

.. code-block:: daml

  data Either a b = Left a | Right b
  data UpDown a b = Up a | Down b

while the following data types are not

.. code-block:: daml

  data Either a b = Left a | Right b
  data Maybe a = Just a | Nothing

``damlc`` provides the command ``generate-generic-src`` to generate modules containing ``Generic``
instances for your data types. For example

.. code-block:: none

  daml damlc generate-generic-src --qualify A --srcdir foo-upgrade-2.0/daml foo-1.0.0/.daml/dist/foo-1.0.0.dar
  daml damlc generate-generic-src --qualify B --srcdir foo-upgrade-2.0/daml foo-2.0.0/.daml/dist/foo-2.0.0.dar

generates two modules ``FooAGenInstances.daml`` and ``FooBGenInstances.daml`` in the
``foo-upgrade-2.0/daml`` directory. This allows us to just use the default ``Convertible`` instances
to do the deep copying:

.. literalinclude:: foo-upgrade-2.0.0/daml/FooWithGenerics.daml
  :language: daml
  :lines: 6-16

You can build this upgrade project with

.. code-block:: none

  daml build --generated-src

If your data types are not isomorphic and the default methods don't compile you have to specify the
conversion manually, for example:

.. literalinclude:: foo-upgrade-2.0.0/daml/FooManual.daml
  :language: daml
  :lines: 4-17
