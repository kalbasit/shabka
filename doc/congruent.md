# What is "declarative congruence"?

There are three system management methods that greatly depend on the
tools that you are using for managing these systems

## Divergence

Setting up and updating a system - be it a workstation or a server -
using manual installation, such as setting a new Mac, or by automated
methods such as `cloud-init` for CoreOS, constitutes a divergent system.
It generally implies bad management of a system, as it diverge from the
baseline overtime and is hard to replicate in case of hardware or
software failure.

![Lisa 2002, Figure 1](https://www.usenix.org/legacy/publications/library/proceedings/lisa02/tech/full_papers/traugott/traugott_html/divergence.gif)

## Convergence

Convergence is the process of mutating a divergent live system back to the
baseline by comparing the state of the system to what the state of the
system should look like and performing automated actions to get the
system back to the baseline.

Puppet, Chef, Ansible and other similar tools follow the convergence
method of maintaining systems. These tools are also considered
declarative as they describe exactly how the system should look like.

![Lisa 2002, Figure 2](https://www.usenix.org/legacy/publications/library/proceedings/lisa02/tech/full_papers/traugott/traugott_html/convergence.gif)

## Congruence

Congruence is the practice of maintaining live hosts in complete
compliance with a declarative baseline of the system. In other words, a
congruent system is a system that you cannot alter without altering the
representation in order to make the changes that you are attempting.

![Lisa 2002, Figure 3](https://www.usenix.org/legacy/publications/library/proceedings/lisa02/tech/full_papers/traugott/traugott_html/congruence.gif)

## Conclusion

Working with a declarative baseline for an infrastructure (whatever the
indented use case of the machine or machines in your network may be)
allows the system to be replicated in a matter of minutes with no manual
configuration or missed steps. It also allows you to know, at any point,
what's the state of your entire infrastructure by simply reading the
declarative baseline of it.

There's an excellent paper by [Steve Traugott and Lance Brown][11] published
in 2002 that goes in details on all three methods.

My hopes with this project is to eliminate the constant need for manual
intervention in order to fix one of my broken machine, for whatever the
reason they broke. Even in the case of hardware failure, all I have to
do is to replace the hardware knowing full well that my entire system
can get back online by simply applying the baseline to a fresh machine
and restore the user data only.

[11]: https://www.usenix.org/legacy/events/lisa2002/tech/full_papers/traugott/traugott.pdf
