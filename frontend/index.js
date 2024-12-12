import * as isometric from '@elchininet/isometric';
import './tensor_playground_web';
import * as d3 from 'd3';

const width = 500;
const height = 320;

function renderTensorDiagram(container, nodes, edges) {
  console.log('renderTensorDiagram', container, nodes, edges);

  const svg = d3.select(container)
    .append("svg")
    .attr("width", width)
    .attr("height", height);

  // Create a simulation for force-directed graph layout
  const simulation = d3.forceSimulation(nodes)
    .force("link", d3.forceLink(edges).id(d => d.id).distance(80))
    .force("charge", d3.forceManyBody().strength(-300))
    .force("center", d3.forceCenter(width / 2, height / 2));

  // Draw the edges (lines)
  const link = svg.append("g")
    .attr("class", "links")
    .selectAll("line")
    .data(edges)
    .enter()
    .append("line")
    .attr("stroke", "#aaa")
    .attr("stroke-width", 2);

  // Draw the nodes (circles) and labels
  const node = svg.append("g")
    .attr("class", "nodes")
    .selectAll("circle")
    .data(nodes)
    .enter()
    .append("g");

  node.append("circle")
    .attr("r", 20)
    .attr("fill", "#69b3a2");

  node.append("text")
    .text(d => d.label)
    .attr("x", 25)
    .attr("y", 5);

  // Update positions on each tick
  simulation.on("tick", () => {
    link
      .attr("x1", d => d.source.x)
      .attr("y1", d => d.source.y)
      .attr("x2", d => d.target.x)
      .attr("y2", d => d.target.y);

    node
      .attr("transform", d => `translate(${d.x}, ${d.y})`);
  });
}

window.renderTensorDiagram = renderTensorDiagram;
window.isometric = isometric;

window.addEventListener('load', () => window.tensor_playground_main());
