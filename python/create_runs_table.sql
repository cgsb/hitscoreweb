CREATE TABLE runs (
  run_id INT UNSIGNED NOT NULL AUTO_INCREMENT,
  PRIMARY KEY (run_id),
  member_id INT UNSIGNED NOT NULL,
  FOREIGN KEY (member_id) REFERENCES members (member_id)
) ENGINE = InnoDB;
