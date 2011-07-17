val kmeans :
  ('a -> 'a -> float) -> (* distance between two points *)
  ('a list -> 'a) -> (* centroid of points *)
  ?initialize_centroids:(int -> 'a array -> 'a array) -> (* select k initial centroids *)
  ?term_iter:int -> (* max iteration *)
  ?term_error:float -> (* terminates if the largest centroid movement in the update is smaller than this value *)
  'a array -> (* points *)
  int -> (* #clusters *)
  int array
