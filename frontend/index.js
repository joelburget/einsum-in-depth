import * as isometric from "@elchininet/isometric";
import "./tensor_playground_web";
import * as d3 from "d3";

const width = 500;
const height = 320;
const tensorNodeDistance = 75;
const edgeNodeDistance = 140;

function renderTensorDiagram(container, nodes, edges) {
  const svg = d3
    .select(container)
    .append("svg")
    .attr("width", width)
    .attr("height", height)
    .attr("viewBox", [-width / 2, -height / 2, width, height])
    .attr("class", "max-w-full h-auto");

  // Separate tensor and edge nodes
  const tensorNodes = nodes.filter((n) => n.type === "tensor");
  const edgeNodes = nodes.filter((n) => n.type === "edge");

  // Position tensor nodes in geometric patterns
  const tensorCount = tensorNodes.length;
  if (tensorCount > 0) {
    tensorNodes.forEach((node, i) => {
      if (tensorCount === 1) {
        node.x = 0;
        node.y = 0;
      } else {
        const angle = (i * 2 * Math.PI) / tensorCount;
        node.x = tensorNodeDistance * Math.cos(angle);
        node.y = tensorNodeDistance * Math.sin(angle);
      }
    });
  }

  // Helper function to find optimal position for edge nodes
  function getEdgeNodePosition(node) {
    const connectedEdges = edges.filter(
      (e) => e.source === node.id || e.target === node.id,
    );

    const connectedTensors = connectedEdges.flatMap((e) => {
      const sourceNode = nodes.find((n) => n.id === e.source);
      const targetNode = nodes.find((n) => n.id === e.target);
      return [sourceNode, targetNode].filter((n) => n.type === "tensor");
    });

    // Calculate average position of connected tensors
    const avgX =
      connectedTensors.reduce((sum, n) => sum + (n.x || 0), 0) /
      connectedTensors.length;
    const avgY =
      connectedTensors.reduce((sum, n) => sum + (n.y || 0), 0) /
      connectedTensors.length;

    // Place node further out in the same direction from center
    const angle = Math.atan2(avgY, avgX);
    return {
      x: edgeNodeDistance * Math.cos(angle),
      y: edgeNodeDistance * Math.sin(angle),
    };
  }

  // Position edge nodes
  edgeNodes.forEach((node) => {
    const pos = getEdgeNodePosition(node);
    node.x = pos.x;
    node.y = pos.y;
  });

  function calculateLinkPath(source, target) {
    if (source === target) {
      const x = source.x;
      const y = source.y;
      return `M${x - 5},${y - 5} A30,30 0 1,1 ${x + 5},${y + 5}`;
    }
    return `M${source.x},${source.y} L${target.x},${target.y}`;
  }

  // Draw the edges
  const link = svg
    .append("g")
    .attr("class", "links")
    .selectAll("path")
    .data(edges)
    .enter();

  link
    .append("path")
    .attr("d", (d) => {
      const source = nodes.find((n) => n.id === d.source);
      const target = nodes.find((n) => n.id === d.target);
      return calculateLinkPath(source, target);
    })
    .attr("fill", "none")
    .attr("stroke", "#aaa")
    .attr("stroke-width", 2);

  link
    .append("text")
    .text((d) => d.label)
    .attr("class", (d) => d.class_name)
    .attr("x", (d) => {
      const source = nodes.find((n) => n.id === d.source);
      const target = nodes.find((n) => n.id === d.target);
      return source === target ? source.x + 30 : (source.x + target.x) / 2;
    })
    .attr("y", (d) => {
      const source = nodes.find((n) => n.id === d.source);
      const target = nodes.find((n) => n.id === d.target);
      return source === target ? source.y - 30 : (source.y + target.y) / 2;
    });

  // Draw the nodes
  const node = svg
    .append("g")
    .attr("class", "nodes")
    .selectAll("g")
    .data(nodes)
    .enter()
    .append("g")
    .attr("transform", (d) => `translate(${d.x}, ${d.y})`);

  node
    .append("circle")
    .attr("r", 10)
    .attr("fill", "#69b3a2")
    .attr("visibility", (d) => (d.type === "edge" ? "hidden" : "visible"));

  node
    .append("text")
    .text((d) => d.label)
    .attr("class", (d) => d.class_name);
}

window.renderTensorDiagram = renderTensorDiagram;
window.isometric = isometric;

window.addEventListener("load", () => window.tensor_playground_main());
