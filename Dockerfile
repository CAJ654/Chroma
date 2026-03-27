# Use the official Shiny Server image from Rocker
FROM rocker/shiny

# Copy the app directory into the Shiny Server directory
COPY app.R /srv/shiny-server/app.R

# Expose the default Shiny Server port
EXPOSE 3838

# Run the Shiny Server
CMD ["/usr/bin/shiny-server.sh"]
