Migration Guide for 1.0
----

Between the last major version (0.3.2.1) and the current major epoch (1.0), many API-related constructs have changed, and I'd like to justify them here and now so that users may have an immortalized explanation for what is most likely a disruptive change to their code.

First, I'd like to say that I don't *like* breaking people's code. As an author and maintainer, I try and make sure that any API breakages are justified either by a significant UX improvement, or by a measurable performance increase large enough to warrant such a breakage. As such, I believe both of these criteria are met by the 0.3.x -> 1.0 upgrade: not only is the API safer to use, but the use of type data to establish the provenance of values encoded by this library also allows the performance-sensitive loops to be much cleaner, eschewing error checking where type data suffices. To prove this point, I've benchmarked the library between these last two epochs. The benchmarks say it all:
