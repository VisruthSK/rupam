# Observation domains

Every column belongs to an observation domain d, the identity of the rows it
was observed on. Combining columns requires equal domains. Filters, joins,
pivots and summaries produce new domains related to their inputs by explicit
maps: an inclusion for a filter, a projection g : d → K for grouping.
Combining columns from different domains is a type error, never a positional
recycle.
