import * as isometric from "@elchininet/isometric";
import "./tensor_playground_web";
import * as d3 from "d3";

const width = 500;
const height = 320;

function renderTensorDiagram(container, nodes, edges) {
  // console.log("renderTensorDiagram", { container, nodes, edges });

  const svg = d3
    .select(container)
    .append("svg")
    .attr("width", width)
    .attr("height", height)
    .attr("viewBox", [-width / 2, -height / 2, width, height])
    .attr("class", "max-w-full h-auto");

  const simulation = d3
    .forceSimulation(nodes)
    .force("charge", d3.forceManyBody().strength(-80))
    .force(
      "link",
      d3
        .forceLink(edges)
        .id((d) => d.id)
        .distance(80)
        .strength(1)
        .iterations(10),
    )
    .force("x", d3.forceX())
    .force("y", d3.forceY())
    .stop();

  function calculateLinkD({ source, target }) {
    const x0 = source.x || 0;
    const y0 = source.y || 0;
    const x1 = target.x || 0;
    const y1 = target.y || 0;
    if (source === target) {
      return `M${x0 - 5},${y0 - 5} A30,30 0 1,1 ${x1 + 5},${y1 + 5}`; // Circular arc for the loop
    }
    return `M${x0},${y0} L${x1},${y1}`;
  }

  simulation.on("tick", () => {
    link.attr("d", calculateLinkD);
    node.attr("transform", (d) => `translate(${d.x}, ${d.y})`);
  });

  simulation.tick(300);

  const link = svg
    .append("g")
    .attr("class", "links")
    .selectAll("path")
    .data(edges)
    .enter();

  link
    .append("path")
    .attr("d", calculateLinkD)
    .attr("fill", "none")
    .attr("stroke", "#aaa")
    .attr("stroke-width", 2);

  link
    .append("text")
    .text((d) => d.label)
    .attr("class", (d) => d.class_name)
    // .attr("class", "text-slate-900 dark:text-white")
    // Offset if it's a loop
    .attr("x", ({ source, target }) =>
      source === target ? source.x + 30 : (source.x + target.x) / 2,
    )
    .attr("y", ({ source, target }) =>
      source === target ? source.y - 30 : (source.y + target.y) / 2,
    );

  const node = svg
    .append("g")
    .attr("class", "nodes")
    .selectAll("circle")
    .data(nodes)
    .enter()
    .append("g")
    .attr("transform", ({ x, y }) => `translate(${x}, ${y})`);

  node
    .append("circle")
    .attr("r", 10)
    .attr("fill", "#69b3a2")
    // .attr("cx", d => d.x)
    // .attr("cy", d => d.y)
    .attr("visibility", (d) => (d.type === "edge" ? "hidden" : "visible"));

  node
    .append("text")
    .text((d) => d.label)
    .attr("class", (d) => d.class_name);
  // .attr("x", 25)
  // .attr("y", 5);
}

window.renderTensorDiagram = renderTensorDiagram;
window.isometric = isometric;

window.addEventListener("load", () => window.tensor_playground_main());
