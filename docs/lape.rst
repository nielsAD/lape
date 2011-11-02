..
    :Author: SRL
    :Date: 2011-09-21
    :Title: Lape Documentation
    :Categories: lape

Welcome to Lape's tutorial!
===========================

.. contents::

Introduction to Lape
====================

Lape is an interpreter for a subset of the Pascal language.
This guide contains a compact introduction to programming and more specifically
programming with Lape. It is by no means a complete guide on learning how to
program. There are awesome books and more comprehensive articles on becoming a
programmer.

This guide serves as reference and compact introduction to lape.

The Basics
==========

Introduction to Programming
----------------------------

Lape features a language very similar to the programming language `Pascal
<http://freepascal.org>`_. Writing a programming language is called
*programming*:

    *The purpose of programming is to create a program that exhibits a certain
    desired behavior.*

To learn programming you have to - as with all things - start with the very
basics. Let us start a very basic but important question: What is a **Program?**
Simply put, a program is a set of logical statements and instructions.
A logical statement is as the name implies - logical. An instruction is simply
an operation the computer will execute - the results of the instruction can vary
- you can make the computer add two numbers, have it draw a line on the screen,
download a file and much more.

Say we have the follow sentence in English:

    *If it is raining I will have to put on my raincoat.*

This roughly translates into the following **code**:

.. code-block:: pascal

    if (it_is_raining) then
        put_on_raincoat();

Now, if you're new to programming, this probably looks very weird. There are
lots of programming language and they all look different - this particular
language is called **Pascal**.

Basically, the line below is a logical construct:

.. code-block:: pascal

    if (it_is_raining)

Whereas the following line is an instruction, in this case an instruction
that tells us to perform other instructions define in the **procedure**
*put_on_raincoat*.

.. code-block:: pascal

    put_on_raincoat()

When combining other logical statements (there are a lot more statements than
just **if**) with instructions, it is possible to construct a **program**.


Hello, Lape!
------------

Now we've gone through a little background, let's start by writing our first
Pascal program.

Observe the following program:

.. code-block:: pascal

    begin
        writeln('hi');
    end.

Immediately we see something new - **begin** and **end**. Every program needs a
**begin** and **end**, it defines where the program *starts* and where it
*stops* - that is when the last instruction has been reached.
Lape will execute instructions as directed by the logical statements until the
end of the program is reached.

So what does the line below really do?

.. code-block:: pascal

    writeln('hi');

To find out, we have to **run** or **execute** the program.

.. NOTE::
    TODO: COVER HOW TO EXECUTE A PROGRAM. (IN SIMBA??)

After *running* the *program*, you should see something like this:

.. code-block:: txt

    Compiled successfully in 70 ms.
    hi
    Successfully executed.

So, the function *writeln* prints something out.
Try changing **'hi'** to something else, if you do it properly, it should print
something else. If it fails after you've changed something - don't worry, just
keep on reading.


Procedures
----------

Now you're thinking, what is a procedure? If we look at the example in
`Hello, Lape!`_,  *writeln* is a **procedure**.

    A **procedure** can be seen as a small program on its own, containing
    instructions and logical statements. When a *procedure* is **invoked**,
    the program executes the instructions and logical statements *in* the
    procedure, which also has a **begin** and **end**. Once the procedure
    *ends*, the program continues as usual.

A simple example:

.. code-block:: pascal

    procedure I_am_a_procedure;
    begin
      writeln('Hello from the procedure');
    end;

    begin
      writeln('Hello from the program');
      I_am_a_procedure();
      writeln('And we are back in the program');
    end.

Here, *I_am_a_procedure* is a procedure, identified by having the word
**procedure** in front of it. Then follows a usual *begin*, followed by some
instructions and terminated with an *end*.

Running this program writes the following:

.. code-block:: txt

    Compiled succesfully in 71 ms.
    Hello from the program
    Hello from the procedure
    And we are back in the program
    Successfully executed.

As you can see, Lape first executes:

.. code-block:: pascal

    writeln('Hello from the program');

And then *invokes* or *calls* the procedure *I_am_a_procedure*, which on its
turn tells Lape to execute the following:

.. code-block:: pascal

    writeln('Hello from the procedure');

And finally, Lape executes:

.. code-block:: pascal

    writeln('And we are back in the program');

As you can see, the *execution* of a program is perfectly logical and follows
very strict steps. A procedure can be used to *divide* code into smaller pieces,
to prevent writing the same instructions and logical constructs over and over.

Now that we have a general idea on how Lape executes your code, we'll move onto
*variables*.

Variables and Types
--------------------

What is a variable? Let us consult Wikipedia [*]_

    In computer programming, a variable is a symbolic name given to some known
    or unknown quantity or information, for the purpose of allowing the name to
    be used independently of the information it represents. A variable name in
    computer source code is usually associated with a data storage location and
    thus also its contents, and these may change during the course of program
    execution.

.. [*]
    https://secure.wikimedia.org/wikipedia/en/wiki/Variable_(computer_science)

In layman terms: We use **variables** in programming to store values for reuse
of the value. There are different types of values: *Variables* are always of a
specific **type**.

Consider the following code:

.. code-block:: pascal

    var
      sum: integer;
    begin
      sum := 0;
      writeln(sum);
      sum := sum + 42;
      writeln(sum);
      sum := sum + 43;
      writeln(sum);
    end.


Here, **sum** is a *variable*. The **type** of *sum* is **Integer** and
**var** indicates that we are about to **declare** one or more variables.

First of all, a *variable* of *type* *Integer* can store whole numbers.
Both positive and negative. There is a limit on the maximum (and minimum)
value of the number, but that is not relevant yet.

So, let us start with:

.. code-block:: pascal

    sum := 0;

This instructions makes the value of *sum* equal *0*, zero.

.. code-block:: pascal

    sum := sum + 42;
    sum := sum + 43;

These two statement successively add 42 and 43 to the value of *sum*, by setting
the value of *sum* to the value of *sum* plus a number.

*Output* of the program is as follows:

.. code-block:: txt

    Compiled succesfully in 71 ms.
    0
    42
    85
    Successfully executed.


Comments
--------

*Code* can include **comments**, comments are used to comment on the code,
typically to make it easier for the reader to understand the code.
Comments on a single line start with *//* and comment that span multiple lines
start with *{* and end with *}*. See the following example:

.. code-block:: pascal

    begin
        writeln('Hello, there'); // This line prints 'Hello, there'

        {
            This is a comment; the writeln that follows in the comments will be
            not executed and treated as comment instead.
            writeln('Hello, I am in a comment');
        }
        writeln('This is no longer a comment');
    end.

If and Else
-----------

So far we've covered how your program is run, what a procedure is, how you can
make simple use of variables and how to comment your code to make it more
readable. Note that we've previously spoken of logical statements, but have not
yet thoroughly discussed them.

The *if* statement consists of a **condition** and one of more
**instructions** that follow the condition:

.. code-block:: pascal

    if condition then
      instruction;

Obviously *condition* must be either **True** or **False**, there is no *maybe*.
Apart from the **Integer** type introduced in `Variables and Types`_, Lape also
supports a **Bollean** type - which can in contrast to an *Integer*, only hold
two kind of values: *True* and *False*.

Thus, the following code is perfectly valid:

.. code-block:: pascal

    var
      a_condition: Boolean;
    begin
        a_condition := True;
        if a_condition then
          writeln('a_condition was true!');
        if a_condition = false then
          writeln('a condition was false!');
    end;

And it will write:

.. code-block:: txt

    a_condition was true!


Lape will try evaluate your *condition* to either True or False.
If it cannot do this, your code is invalid. As a result, you can combine logical
constructs if then evaluate to either *True* or *False*. See `Not, And, Or,
Xor`_ on combining logical constructs.

For example, the following code is **invalid**:

.. code-block:: pascal

    if 42 then
      writeln('The answer to life, the universe and everything!');


The **if** statement can optionally make use of an **else** clause,
simplifying our previous code:

.. code-block:: pascal

    var
      a_condition: Boolean;
    begin
        a_condition := True;
        if a_condition then
          writeln('a_condition was true!') // Note that there is no semicolon here now
        else
          writeln('a condition was false!');
    end;

If you wish to perform more than one operation in you *if* statement, use the
**begin** and **end** keywords:

.. code-block:: pascal

    var
      a_condition: Boolean

    begin
        a_condition := True;
        if a_condition then
        begin
          writeln('a_condition is true');
          writeln('this is another procedure call');
        end
        else
        begin
          writeln('a_condition is false');
          writeln('This line and the line above will however never be printed');
        end;


Not, And, Or, Xor
-----------------

Lape contains a few special *logical operators*: **Not**, **And**, **Or** and
**Xor**.

Not
~~~

The **Not** instruction negates the value that follows it:

.. code-block:: pascal

    var a_boolean: boolean;

    begin
       a_boolean := not False;
       if a_boolean then
         writeln('a_boolean is True');
    end;


A simple **Truth Table** for the **not** operator:

=========  =====
not True   False
not False  True
=========  =====

And
~~~

The **And** operator takes two logical values (or constructs that *evaluate*
to a value) and *evaluates* to *True* if both these values are *True*, otherwise
it evaluates to *False*.

Truth table for the **and** operator (treat A and B as logical
values/constructs):

=====  =====  =======
   Inputs     Output
------------  -------
  A      B    A and B
=====  =====  =======
False  False  False
True   False  False
False  True   False
True   True   True
=====  =====  =======


Or
~~

The **Or** operator takes two logical values (or constructs that *evaluate*
to a value) and *evaluates* to *True* if any of these values are *True*,
otherwise it evaluates to *False*.

Truth table for the **or** operator (treat A and B as logical
values/constructs):

=====  =====  ======
   Inputs     Output
------------  ------
  A      B    A or B
=====  =====  ======
False  False  False
True   False  True
False  True   True
True   True   True
=====  =====  ======


Xor
~~~

=====  =====  =======
   Inputs     Output
------------  -------
  A      B    A xor B
=====  =====  =======
False  False  False
True   False  True
False  True   True
True   True   False
=====  =====  =======


Variables cont.
---------------

The section `Variables and Types`_ gave a very basic introduction on variables,
just enough to get you to this section. First, we'll introduce a few more basic
**types**:

.. code-block:: pascal

    var
        b: boolean;
        s: string;
        i: integer;

    begin
        b := True or False; // b = True
        s := 'Hello, ' + 'World'; // Hello, World
        i := 24 * 2;
    end;

We've introduced one new *type*:

    -   A **string** consists of one or more characters, denoted by the
        surrounding **'**.

As a short reminder:

    -   A **boolean** can either hold the value *True* or *False*.
    -   An **integer** holds a number.

Now, looking at the previous code, we notice the **or** operator being applied
to *True* and *False*, but this is nothing new.

Moving on, we can see that the *string* **s** is being given the value
*'Hello, ' + 'World'*. What does an addition of two *strings* even mean?
In this case, the two strings *'Hello, '* and *'World'* are *combined* to
*'Hello, World'*. The **+** operator is not defined for every type: it mainly
works on strings and integers.

The last statement takes the number *24* and **multiplies** it by *2*.


Logical evaluation and parentheses
----------------------------------


Functions
---------


Records in Lape
---------------

Loops in Lape
-------------

Arrays in Lape
--------------

Pointers in Lape
----------------

Classes in Lape
---------------

Lape Reference
==============

BaseTypes
---------

    -   Integers(Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64)
    -   Floats(Extended,Double,Single,Currency)
    -   Chars(Ansi,Wide)
    -   Strings(Short,Ansi,Wide,Unicode)
    -   Booleans(Boolean, ByteBool, WordBool, LongBool)
    -   Variants
    -   Arrays(static, dynamic)
    -   record
    -   union
    -   enums
    -   sets
    -   pointers
    -   function pointers

Operators
---------


Assignment Operator
~~~~~~~~~~~~~~~~~~~

    -   :=

.. code-block:: pascal

    var
      b: boolean;
      a: integer;
      s: string;
      sa: array of string;

    begin
        b := True;
        a := 41+1;
        s := 'This parrot is an ex-parrot';
        setlength(sa, 4);

        // Pick the odd one out
        sa[0] := 'This parrot is no more';
        sa[1] := 'It is pleading demise';
        sa[2] := 'If you had not nailed him to the perch he would be pushing up the daisies!';
        sa[3] := 'My hovercraft is full of eels';
    end;

The assignment operator assigns a *value* to a *variable*.

Equality Operators
~~~~~~~~~~~~~~~~~~

Equality
::::::::

The **=** operator checks for equality of two basic types of the same type:

.. code-block:: pascal

    if (42 = 42) and ('God is God' = ('God is' + ' God') ) then
      writeln('Everything equals itself, surprisingly');

Inequality
::::::::::

The **<>** operator is similar to the **=** operator, but checks for inequality
rather than equality:

.. code-block:: pascal

    if '42' <> 'The answer to life, the universe and everything' then
      writeln('Blasphemy!');

Greater than
::::::::::::

The **>** operator returns true if the first value is greater than the second
value:

.. code-block:: pascal

    if 43 > 42 then
      writeln('42 is larger than 43');

Greater than or equals
:::::::::::::::::::::::

The **>=** operator returns true if the first value is greater than *or equal
to* the second value:

.. code-block:: pascal

    if (43 >= 42) and (42 >= 42) then
      writeln('42 is equal to or larger than both 42 and 43');

Lesser than
:::::::::::

The **<** operator returns true if the first value is smaller than the second
value:

.. code-block:: pascal

    if 42 < 43 then
      writeln('42 is smaller than 43');

Lesser than or equals
~~~~~~~~~~~~~~~~~~~~~

The **<=** operator returns true if the first value is smaller than *or equal
to* the second value:

.. code-block:: pascal

    if (42 <= 42) and (42 <= 43) then
      writeln('42 is smaller than or equal to 42 and 43');

Pointer Operators
~~~~~~~~~~~~~~~~~

    -   @
    -   ^

The **@** operator returns the memory address of a variable, whereas the **^**
operator retrieves the value from a memory address:

.. code-block:: pascal

    var
        ip: ^Integer;
        i: Integer;

    begin
        i := 41;
        ip := @i;
        ip^ := ip^ + 1;
        writeln(i);
    end;

Mathematical Operators
~~~~~~~~~~~~~~~~~~~~~~

Addition
::::::::

The **+** operator performs an addition, note that it does not have to be a
numerical addition.
The **+** operator can be defined for other types with `Operator Overloading`_.

.. code-block:: pascal

    writeln(40+2); // Integer addition adds two numbers.
    writeln('Hello, ' + ' World'); // String addition concatenates strings

.. TODO: On arrays? Custom types?


Subtraction
:::::::::::

The **-** operator performs a subtraction:

.. code-block:: pascal

    writeln(44-2); // Integer substraction.

.. TODO: Arrays? Custom types?

Multiplication
~~~~~~~~~~~~~~

The **\*** operator


.. ***

Division
::::::::

    -   /
    -   DIV

The **/** operator performs a division:

.. code-block:: pascal

    writeln(84/2);
    writeln(1/3);
    writeln(1/3.0);
.. TODO: Vershil tussen DIV en /

Power Operator
::::::::::::::

The **\*\*** operator performs the mathematical power operation:

.. ***

.. math::

     a^b

Where *a* is the first value; and *b* the second value:

.. code-block:: pascal

    writeln(2**8);

Logical Operators
~~~~~~~~~~~~~~~~~

Logical AND
:::::::::::

The **AND** operator takes two logical values (or constructs that *evaluate*
to a value) and *evaluates* to *True* if both these values are *True*, otherwise
it evaluates to *False*.

Truth table for the **AND** operator (treat A and B as logical
values/constructs):

=====  =====  =======
   Inputs     Output
------------  -------
  A      B    A and B
=====  =====  =======
False  False  False
True   False  False
False  True   False
True   True   True
=====  =====  =======

Logical OR
::::::::::

The **OR** operator takes two logical values (or constructs that *evaluate*
to a value) and *evaluates* to *true* if any of these values are *true*,
otherwise it evaluates to *false*.

Truth table for the **OR** operator (treat A and B as logical
values/constructs):

=====  =====  ======
   Inputs     Output
------------  ------
  A      B    A or B
=====  =====  ======
False  False  False
True   False  True
False  True   True
True   True   True
=====  =====  ======

Logical XOR
:::::::::::

The **XOR** operator takes two logical values (or constructs that *evaluate*
to a value) and *evaluates* to *True* if *(only) one* of these values is true
otherwise it evaluates to *False*.

Truth table for the **XOR** operator (treat A and B as logical
values/constructs):

=====  =====  =======
   Inputs     Output
------------  -------
  A      B    A xor B
=====  =====  =======
False  False  False
True   False  True
False  True   True
True   True   False
=====  =====  =======

Logical NOT
:::::::::::

The **NOT** instruction negates the value that follows:

=========  =====
not True   False
not False  True
=========  =====

Bit Operators
~~~~~~~~~~~~~

Bitwise NOT
:::::::::::

The **NOT** operator inverts all the bits.

.. code-block:: pascal

    writeln(not -43);
..

Bitwise AND
:::::::::::

The **AND** operator performs a logical AND operation on each bit of the two
input numbers.

.. code-block:: txt

    001010 and 101010 = (0 and 1)(0 and 0)(1 and 1)(0 and 0)(1 and 1)(0 and 0) = 001010

Usage:

.. code-block:: pascal

    writeln(10 and 42); // results in 10
..

Bitwise OR
::::::::::

The **OR** operator performs a logical OR operation on each bit of the two
input numbers.

.. code-block:: pascal

    writeln(32 or 10); // results in 42
..

Bitwise XOR
:::::::::::

The **XOR** operator performs a logical XOR operation on each bit of the two
input numbers.

.. code-block:: pascal

    writeln(5 xor 5);
..


Bitwise SHL
:::::::::::

The **SHL** operator performs a left shift by n bits on the first value, where n
is the second value.

.. code-block:: pascal

    writeln(21 shl 1);
    // 21 = '0b10101'
    // 42 = '0b101010'
..

Bitwise SHR
:::::::::::

The **SHR** operator performs a right shift by n bits on the first value, where
n is the second value.

.. code-block:: pascal

    writeln(84 shr 1);
..

Getting and Setting individual bits
:::::::::::::::::::::::::::::::::::


Functions and Procedures
------------------------



Functions
~~~~~~~~~





Procedures
~~~~~~~~~~



Function and Procedure overloading
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


Default Parameters
~~~~~~~~~~~~~~~~~~


Records and Custom Types
-------------------------

Custom Types
~~~~~~~~~~~~

Records
~~~~~~~

Operator Overloading
~~~~~~~~~~~~~~~~~~~~


Nested Declarations
-------------------


Loops
-----

For Loop
~~~~~~~~

Repeat-Until Loop
~~~~~~~~~~~~~~~~~

While-Do Loop
~~~~~~~~~~~~~

Pointers
--------

Exceptions
----------


