Y = lambda { |st|           # startup
      lambda { |pg|         # pg
        pg.(pg)
      }.(
        lambda { |f|        # sidestep f, inject a nicer one
          st.(lambda { |x| f.(f).(x) })
        }
      )
    }

Y =
  ->(st) {
    ->(st) { st.(st) }.(
      ->(f) {
        st.(->(x) { f[f][x] })
      }
    )
  }

F =
  ->(f) {
    ->(n) {
      if n.zero?
        1
      else
        n * f[n-1]
      end
    }
  }

Y[F][10]
