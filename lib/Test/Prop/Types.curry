-------------------------------------------------------------------------
--- This module defines some types used by the EasyCheck libraries.
---
--- @author Michael Hanus
--- @version January 2019
-------------------------------------------------------------------------

module Test.Prop.Types where

-- The types of properties:

--- Abstract type to represent properties involving IO actions.
data PropIO = PropIO (Bool -> String -> IO (Maybe String))

--- Abstract type to represent standard properties to be checked.
--- Basically, it contains all tests to be executed to check the property.
data Prop = Prop [Test]

-------------------------------------------------------------------------
--- Abstract type to represent a single test for a property to be checked.
--- A test consists of the result computed for this test,
--- the arguments used for this test, and the labels possibly assigned
--- to this test by annotating properties.
data Test = Test Result [String] [String]

--- Data type to represent the result of checking a property.
data Result = Undef | Ok | Falsified [String] | Ambigious [Bool] [String]

-------------------------------------------------------------------------
